{-# LANGUAGE FlexibleContexts, MultiWayIf, LambdaCase #-}
{-# LANGUAGE RecursiveDo, OverloadedStrings  #-}
module StgToLLVM (stgToIRTop) -- :: [STGTopBinding] -> LLVM.Module
where

-- Local imports
import StgSyn

-- LLVM
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction

import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Typed as T
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as P
import qualified LLVM.AST.FloatingPointPredicate as Pf
import LLVM.AST.Global
import LLVM.AST.Typed
import LLVM.AST.Linkage

--import Data.Vector (Vector, empty, (!), snoc)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Fix (MonadFix)
import Control.Monad.Fail (MonadFail)

import Data.Foldable (find)
import Data.Maybe (fromJust, isJust)
import GHC.Exts (groupWith)
import qualified Data.ByteString
import Data.ByteString.Short (toShort)

import Debug.Trace

-- StgToLLVM :: STG continuation passing style -> llvm SSA style
--
-- ! Do not confuse codegen semantics and codegened code semantics !
-- It is difficult to be crystal clear within the comments.
--
-- **********************************************
-- * Operational semantics of generated llvm IR *
-- **********************************************
-- 0. Only functions are legal free variables in llvm (we don't use globals)
-- The module conatains a closure (function) for each non trivial binding,
-- and each top level binding
-- 
-- 1. Applications 
-- LLVM functions must be saturated when called.
-- we use the push-enter model: the caller handles function arity.
-- (as opposed to eval-apply, where functions are responsible for their arities)
-- this is all fixed during codegen, in llvm all applications are saturated
-- Note. llvm functions are generated for non-trivial (growable) let bindings,
-- all free variables that an llvm fn needs are explicitly passed as args.

-- 2. literals (0 arity functions or raw values or let bound identifiers)
--
-- 4. Case expressions and data constructors
-- value cases are just llvm switch instructions
-- Data constructors are desugared to this form before codegen,
-- or maybe it was possible to avoid the case and resolve to a direct jump.
--
-- **************
-- * LLVM notes *
-- **************
-- We can mark most llvm functions as 'pure', which is very helpful to llvm.
-- most useful LLVM optimization passes:
-- -reassociate       -aims to improve constant propagation
-- -block-placement   -rearrange switch blocks in case expressions
-- -simplifycfg
-- -inline
-- -jump-threading    -branch analysis
-- -tailcallelim
--
-- Worked example:
-- STG: fib n = case n of { 0->0; 1->1; _-> fib (n-1) fib (n-2) }
-- LLVM (no optimizations):
-- 
-- define external ccc  double @fib(double  %a)    {
-- ; <label>:0:
--   switch double %a, label %1 [double 0.000000e0, label %7 double 1.000000e0, label %8] 
-- ; <label>:1:
--   %2 = fsub double %a, 1.000000e0 
--   %3 =  call ccc  double  @fib(double  %2)  
--   %4 = fsub double %a, 2.000000e0 
--   %5 =  call ccc  double  @fib(double  %4)  
--   %6 = fadd double %3, %5 
--   br label %9 
-- ; <label>:7:
--   br label %9 
-- ; <label>:8:
--   br label %9 
-- ; <label>:9:
--   %10 = phi double [%6, %1], [0.000000e0, %7], [1.000000e0, %8] 
--   ret double %10 
-- }


-- ******************************************
-- * Semantics of code gen :: STG -> LLVMIR *
-- ******************************************
--
-- codegen happens within llvm-hs-pure ModuleBuilder and IRBuilder monads.
-- StgCase uses mdo (MonadFix) to generate phi blocks
-- stgToIR also relies on a MonadState StgToIRState
--
-- StgToIRState:
-- maps the bindings available at any given stack frame,
--   (also locally bound arguments in a function)

-- Continuations are either basicblocks or functions.
-- ContBB can simply be jumped to, ContFn must be passed args and freevars.
-- Note. free Operands must be passed explicitly, but not functions; they're all globals.
data Cont
  = ContLi LLVM.AST.Operand       -- an SSA - can be passed through llvm functions.
  | ContFn LLVM.AST.Operand       -- a function. (Note. llvm calls are alway saturated)
  | ContRhs StgRhs                -- Placeholder (we can let-bind this immediately)
data StgToIRState = StgToIRState 
  { bindMap      :: Map.Map StgId Cont -- llvm things available (at a given stack frame)
  }

-- stgToIRTop:  This module's only export
stgToIRTop :: [StgBinding] -> LLVM.AST.Module
stgToIRTop topBindings =
   let stgToIRState = StgToIRState
           { bindMap = Map.empty
           }
       go = mapM genTopBinding topBindings
       genTopBinding (StgBinding iden rhs) = fnToLlvm iden rhs
   in evalState (buildModuleT "MainModule" go) stgToIRState

-- We have to make a function for every top level binding, so it's useful to define this here.
fnToLlvm iden (StgRhsClosure flag args argTypes retty body) = mdo
    --let params = map (\ty -> (ty, NoParameterName)) argTypes
    let params = map (\ty -> (ty, ParameterName "a")) argTypes
    modify (\x->x { bindMap = Map.insert iden (ContFn f) (bindMap x) })
    f <- function (Name $ toShort iden) params retty $ \llvmArgs ->
        -- fns bodies handled as: let args in fn
        let argBinds = zipWith StgBinding ((\(StgVarArg a) -> a) <$> args) -- ! litargs
                                          (StgRhsSsa <$> llvmArgs)
        in do -- sv State, codeGen a let, then clear the argument bindings
        svState <- get -- function args may squash bindings, so save and restore.
        ret =<< (stgToIR $ StgLet argBinds body)
        put svState
    return f
-- It must be a constant operand, this isn't checked
fnToLlvm iden (StgRhsSsa ssa) = function (Name $ toShort iden) [] (typeOf ssa) $ \[] -> ret ssa

-- a let, but without the 'in expr' (for top bindings)
registerBindings bindList =
  let (stgIds, rhsList) = unzip $ (\(StgBinding i rhs) -> (i, rhs)) <$> bindList
      insertfn id cont = modify (\x->x { bindMap = Map.insert id cont (bindMap x) })
   in zipWithM_ insertfn stgIds (ContRhs <$> rhsList)

-- ***********************************
-- * StgToIR :: STGExpr -> m Operand *
-- ***********************************
-- StgToIR: Goal is to codegen how to reduce an StgExpr to a value.
stgToIR :: (MonadState (StgToIRState) m, MonadModuleBuilder m, MonadIRBuilder m, MonadFix m)
  => StgExpr -> m Operand

-- StgLet: register the bindings and let the 'expr' codegen them when it needs them.
stgToIR (StgLet bindList expr) = registerBindings bindList >> stgToIR expr

-- literals/functions with 0 arity
stgToIR (StgLit lit) = case lit of
     StgLitArg l -> return $ ConstantOperand l
     StgVarArg i -> stgToIR $ StgApp i [] -- varArgs are 0 arity functions
     StgExprArg e -> stgToIR $ e

-- PrimOps = extract raw values and emit the primop's llvm instruction.
stgToIR (StgPrimOp (StgPrimBinOp op a b)) = do -- Second easiest case, primitives
  a <- (stgToIR a)
  b <- (stgToIR b)
  emitInstr (typeOf a) $ op a b

-- StgApp = arity >0 Function application (including Constructors)
-- Arities always exactly match
stgToIR (StgApp iden args) = do -- function application
  -- prepare arguments.
  ssaArgs <- mapM (stgToIR . StgLit) args
  let params = (\x->(x,[])) <$> ssaArgs -- the empty list is the parameter attributes
  -- Get the function. It might not yet be codegened, but should exist in our bindMap.
  -- return $! traceShow iden ssaArgs
  gets (fromJust . (Map.!? iden) . bindMap) >>= \case
      ContLi f -> error "StgToIR ($): panic: unexpected literal"
      ContFn f -> call f params
      ContRhs (StgRhsSsa val) -> return val
      ContRhs rhs@(StgRhsClosure _ _ _ _ _) -> do
          traceM "error"
          f <- fnToLlvm iden rhs
          call f params

-- | case: produce a switch on a literal value
stgToIR (StgCaseSwitch expr defaultBranch alts) = mdo
  let (vals, altCode) = unzip alts
  scrut <- stgToIR expr
  let genAlts expr = do -- collect (result,block) pairs to emit the phi instr
         b <- block
         r <- stgToIR expr
         br endBlock
         return (r, b)
  switchAlts <- return $ zip vals (snd $ unzip retBlockPairs)
  switch scrut dBlock switchAlts
  dBlock <- block
  dSsa <- stgToIR defaultBranch <* br endBlock
  retBlockPairs <- mapM genAlts altCode
  endBlock <- block
  phi $ (dSsa, dBlock) : retBlockPairs

-- A (de)constructor
--  AlgAlt pattern -> do
--      let ContBB cont = findPatternMatch pattern ((\(AltCon c) -> c) <$> alts)

--      -- clear alts from the contStack
--      modify (\x -> x { patternStack = drop (length alts) $ patternStack x })
--      br cont
--      gets r1

-- Used by stgCase to find the matching constructor in a let binding
-- findPatternMatch :: DataCon -> [(DataCon, Cont)] -> Cont
-- findPatternMatch this list = cont
--   where
--   (match, cont) = fromJust $ find (findPredicate this) (list) -- theres always a match
--   allMatch :: [DataCon] -> [DataCon] -> Bool
--   allMatch l1 l2 = Prelude.and $ zipWith p l1 l2
--   findPredicate b (a,_) = p a b
--   p :: DataCon -> DataCon -> Bool
--   p a b = case a of
--     Unboxed x -> case b of { Unboxed y -> x == y ; _ -> False }
--     ProductTypeCon x -> case b of { ProductTypeCon y -> allMatch x y ; _ -> False }
--     SumTypeCon     x -> case b of { SumTypeCon y -> allMatch x y ; _ -> False }

    ---- Interact with argstack to apply the right number of args
    --useArgStack args arity = let l = length args in if
    --  | l == arity -> args
    --  | l > arity -> do
    --    let (fargs, rem) = splitAt arity args
    --    modify (\x -> x { argStack = rem ++ argStack x })
    --    fargs
    --  | l < arity -> do
    --    let missing = arity - l
    --        fargs = gets (take missing . argStack)
    --    modify (\x -> x { argStack = drop missing $ argStack x })
    --    fargs
