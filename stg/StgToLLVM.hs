{-# LANGUAGE
  FlexibleContexts, MultiWayIf, LambdaCase, RecursiveDo, OverloadedStrings,
  ConstraintKinds, NoMonomorphismRestriction, RankNTypes, TupleSections,
  StrictData
#-}
-- {-# OPTIONS -Wall #-}
module StgToLLVM (stgToIRTop) -- :: [STGTopBinding] -> LLVM.Module
where

-- Local imports
import StgSyn
import CodegenRuntime
import DataToLLVM
import LlvmHsExts (globalStringPtr, constZero, charPtrType, unknownPtrType, sizeof)

-- LLVM
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction hiding (globalStringPtr)

import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
--import qualified LLVM.AST.IntegerPredicate as P
--import qualified LLVM.AST.FloatingPointPredicate as Pf
import LLVM.AST.Global
import LLVM.AST.Linkage
import LLVM.AST.Typed
import LLVM.AST.AddrSpace
import LLVM.AST.Attribute

import Data.List (elemIndex) -- to match deconstruction orders
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Fix (MonadFix)
import Data.Functor.Identity (Identity)

import Debug.Trace
import Control.Exception (assert)

-- StgToLLVM :: STG continuation passing style -> llvm SSA style
--
-- ! Do not confuse codegen semantics and codegened code semantics !
-- It is difficult to be crystal clear within the comments.
--
-- **********************************************
-- * Operational semantics of generated llvm IR *
-- **********************************************
-- Free variables
-- Only functions (and globals) are legal free variables in llvm, they reside in module scope
-- The module contains a function for all bindings, with the exception of local constants.
-- Stack frames are responsible for making sure these are called at most once.
--
-- Applications 
-- LLVM functions must be saturated when called, partial apps are illegal at this stage
--
-- Case expressions
-- value cases are llvm switch instructions
-- data cases use the fns spawned when data declarations are first encountered.
--
-- Stack Frames ~= function layers
-- A key concept in the execution model
-- 1. responsible for data
-- 2. responsible for let bindings: these are fns that should be called at most once

-- stgToIRTop:  This module's only export
stgToIRTop :: [StgTopBinding] -> LLVM.AST.Module
stgToIRTop topBindings =
   let stgToIRState = StgToIRState
           { bindMap      = Map.empty
           , dataFnsMap   = Map.empty
           , typeMap      = Map.empty
           , stackFrame   = Nothing
           , subData      = False
           }
       rtsExterns = [("malloc", [IntegerType 32], charPtrType)
                    ,("free",   [charPtrType],    VoidType)
                    ,("error",  [IntegerType 32, IntegerType 32, charPtrType], VoidType)
                    ]

       emitCustomRtsFns = do
          let z = ConstantOperand $ C.Int 32 0
              noMatchFnName = "NoPatternMatchError"
          failStr <- globalStringPtr "No default switchCase alt" "noPatternMsg"
          casePanicFn <- function noMatchFnName [] VoidType $ \[] -> do
              errFn <- gets ((\(Just (ContFn f)) -> f) . (Map.!? "error") . bindMap)
              call errFn [(z,[]), (z,[]), (ConstantOperand failStr,[])]
              unreachable
          modify (\x->x{ bindMap = Map.insert noMatchFnName (ContFn casePanicFn) (bindMap x) })
          return casePanicFn

       emitRtsDeclarations =
         let go (fname, argTys, retTy) = extern fname argTys retTy >>= \f ->
               modify (\x->x{ bindMap = Map.insert fname (ContFn f) (bindMap x) })
               >> return f
         in mapM go rtsExterns

       genTopBinding (StgTopBind (StgBinding iden rhs)) = fnToLlvm iden rhs
       genTopBinding (StgTopData sumType) = genConstructor sumType
       genTopBinding (StgTopTypeDef nm ty) =
           modify (\x->x { typeMap = Map.insert nm ty (typeMap x) }) -- >> typedef nm Nothing
             >> return (ConstantOperand C.TokenNone) -- we must (?!) return an Operand

       emitModule :: CodeGenModuleConstraints m => m [Operand]
       emitModule = let order (StgTopBind _) = True
                        order _ = False
                        bindings = (\(StgTopBind b) -> b) <$> filter order topBindings
                    in registerBindings bindings -- handle forward references
                    *> emitRtsDeclarations
                    *> emitCustomRtsFns
                    *> mapM genTopBinding topBindings
       -- Override buildModuleT so we get more control over module parameters
       --myBuildModuleT :: ShortByteString -> ModuleBuilderT m a -> Module
       myBuildModuleT nm = fmap mkModule . execModuleBuilderT emptyModuleBuilder
         where mkModule ds = defaultModule { moduleName = nm
                                           , moduleDefinitions = ds
                                      --   , moduleTargetTriple = Just "x86_64-pc-linux-gnu"
                                           }
   in evalState (myBuildModuleT "MainModule" emitModule) stgToIRState

-- suspicious function.. but sometimes a vanilla llvmType/alias is expected
-- eg in an extern declaration we cannot really accept anything more complex (?!)
mkLlvmTy :: (CodeGenModuleConstraints m)
  => StgType -> m LLVM.AST.Type
mkLlvmTy = \case
  StgLlvmType t -> return t
  StgTypeAlias iden -> gets (aliasToType iden . typeMap) >>= \case
    Nothing -> error ("unknown type alias: " ++ show iden)
    Just ty -> mkLlvmTy ty
  any -> error "expected vanilla llvm type or type alias"

-- Check if a type is stg data, which stg has to handle specially (manage it's memory)
isStgData :: CodeGenModuleConstraints m => StgType -> m (Maybe C.Constant)
isStgData (StgTypeAlias iden) = gets ((Map.!? iden) . dataFnsMap) >>= \case
                                    Nothing -> maybe (return Nothing) isStgData =<<
                                                 gets (aliasToType iden . typeMap)
                                    Just datafns -> return (Just $ staticSize datafns)
isStgData _ = return Nothing

-- fnToLlvm: Find the type of the function, emit it and hand over to stgToIR for the body
-- This is relatively straightforwards, just remember that functions returning data
-- need an extra parameter: a pointer to stack memory they can use,
-- and functions calling these need to pass stack memory (from their frame or a prev one !)
fnToLlvm :: (CodeGenModuleConstraints m)
  => StgId -> StgRhs -> m Operand
fnToLlvm iden (StgRhsClosure args argTypes stgRetType body) = mdo
  maybeData <- isStgData stgRetType
  retTy <- getType stgRetType
  params  <- map (, ParameterName "a") <$> mapM getType argTypes

  -- register the fn before codegen in case it refs itself
  case maybeData of
    Just sz -> modify (\x->x{dataFnsMap=Map.insert iden (ProxyDataFn sz f) (dataFnsMap x)})
          >> modify (\x->x{bindMap = Map.delete iden (bindMap x)}) -- otherwise inf loop..
    Nothing -> modify (\x->x {bindMap = Map.insert iden (ContFn f) (bindMap x)})

  svState <- get -- function args may squash bindings, so save and restore.
  let getArgBinds llvmArgs = zipWith StgBinding ((\(StgVarArg a) -> a) <$> args)
                                                (StgRhsSsa <$> llvmArgs)
      memParam = (charPtrType, ParameterName "mem")
 -- ~= let args in fnbody
  f <- case maybeData of
        Just _ -> function iden (memParam : params) retTy $ \(mem : llvmArgs) -> do
                    modify (\x->x { stackFrame=Just $ StackFrame (C.Int 32 8) mem []} )
                    ret =<< stgToIR (StgLet (getArgBinds llvmArgs) body)

        Nothing -> function iden params retTy $ \llvmArgs ->
                     ret =<< stgToIR (StgLet (getArgBinds llvmArgs) body)
  put svState
  return f

-- StgRhsSsa is used internally for binding fn args, should never appear here
fnToLlvm iden (StgRhsSsa ssa)   = error "refusing to generate a function for an ssa binding"
fnToLlvm iden (StgRhsConst ssa) = global iden (LLVM.AST.Typed.typeOf ssa) ssa
-- extern functions (also var args)
fnToLlvm iden (StgExtern argTys retTy)   = externDecl iden extern        argTys retTy
fnToLlvm iden (StgExternVa argTys retTy) = externDecl iden externVarArgs argTys retTy
-- llvm fn (extern|externVarArgs) and emit extern declaration from name and types.
-- externDecl: used only by the above 2 lines
externDecl iden declExternFn argTys retTy = do
  argTys' <- mapM mkLlvmTy argTys
  retTy' <- mkLlvmTy retTy
  f <- declExternFn iden argTys' retTy'
  modify (\x->x { bindMap = Map.insert iden (ContFn f) (bindMap x) })
  return f

-- let bindings without the 'in expr', doesn't codegen anything
registerBindings :: MonadState StgToIRState m
  => [StgBinding] -> m ()
registerBindings bindList =
  let (stgIds, rhsList) = unzip $ (\(StgBinding i rhs) -> (i, rhs)) <$> bindList
      insertfn nm cont = modify (\x->x { bindMap = Map.insert nm cont (bindMap x) })
   in zipWithM_ insertfn stgIds (ContRhs <$> rhsList)

-- ***********************************
-- * StgToIR :: STGExpr -> m Operand *
-- ***********************************
-- StgToIR: Goal is to codegen how to reduce an StgExpr to a value.
stgToIR :: (MonadState StgToIRState m, MonadModuleBuilder m, MonadIRBuilder m, MonadFix m)
  => StgExpr -> m Operand

-- StgLet: register the bindings and let the 'expr' codegen them when it needs them.
stgToIR (StgLet bindList expr) = registerBindings bindList >> stgToIR expr

-- literals/functions with 0 arity
-- One might assume this is trivial.. but we must:
-- * emit constant arrays as globals
-- * call 0 arity functions (thunks)
-- * (maybe) launch threads for applications
-- * prepare stack memory for args returning data
-- * call destructors for args that aren't returned
stgToIR (StgLit lit) =
  let globalArray :: CodeGenIRConstraints m => C.Constant -> m C.Constant
      globalArray arr@C.Array{} = -- const arrays need to be emmited in global scope
          let ty = LLVM.AST.Typed.typeOf arr
          in do
             nm <- fresh
             emitDefn $ GlobalDefinition globalVariableDefaults
               { name = nm
               , LLVM.AST.Global.type' = ty
               , linkage = Internal
               , isConstant = True
               , initializer = Just arr
               , unnamedAddr = Just GlobalAddr
               }
             return $ C.GetElementPtr True (C.GlobalReference (ptr ty) nm) zz
                 where zz = [C.Int 32 0, C.Int 32 0]
      globalArray notArray = return notArray

      -- some literals are in fact 0 arity functions that must be resolved.
      callIfThunk :: CodeGenIRConstraints m => Operand -> m Operand
      callIfThunk s = case LLVM.AST.Typed.typeOf s of
          FunctionType _ [] _                 -> call s []
          PointerType (FunctionType _ [] _) _ -> call s []
          dont_call                           -> return s

      handle0Arity :: CodeGenIRConstraints m => StgId -> Symbol -> m Operand
      handle0Arity iden = \case
          ContLi s  -> return s --  never call literals
          ContFn v  -> callIfThunk v
          ContRhs (StgRhsSsa val) -> return val -- don't make a function for literals
          ContRhs r -> fnToLlvm iden r >>= callIfThunk -- rly

  in case lit of
     StgConstArg l ->  ConstantOperand <$> globalArray l
     StgSsaArg s ->  return s
     -- some varArgs are constructors or lazy closures that we must resolve (call) now.
     StgVarArg i ->  gets ((Map.!? i) . bindMap) >>= \case
                         Just v -> handle0Arity i v
                         Nothing -> stgConApp i []
                         -- Nothing -> error (show i ++ ": variable not found in bindMap")
     StgExprArg e -> stgToIR e

-- PrimOps = extract raw values and emit the primop's llvm instruction.
-- Might spawn a thread if at least one side is a function application
stgToIR (StgPrimOp (StgPrimBinOp op a b)) = do
  a' <- stgToIR a
  b' <- stgToIR b
  emitInstr (typeOf a') $ op a' b'

-- StgApp
-- Arities always exactly match
-- Recall stack frames are responsible for all data returned by their functions !
-- We may need to alloca some stack space and pass that to the function
-- The fn might not yet be codegened, but should still be bound
stgToIR (StgApp iden args) =
  gets ((Map.!? iden) . bindMap) >>= \case
    Just x -> do -- Try the bindMap (regular/vanilla llvm fns)
        params <- map (,[]) <$> mapM (stgToIR . StgLit) args
        (`call` params) =<< case x of
            ContLi f -> error "StgToIR ($): panic: unexpected literal"
            ContFn f -> return f
            ContRhs (StgRhsSsa val) -> return val -- a function pointer arg to call
            ContRhs rhs@StgRhsClosure{} -> fnToLlvm iden rhs
            ContRhs _ -> error "cannot call non function"
    Nothing -> stgConApp iden args -- Try the Data map

-- | case: produce a switch on a literal value
stgToIR (StgCaseSwitch expr defaultBranch alts) = mdo
  let (vals, altCode) = unzip alts
  scrut <- stgToIR expr `named` "scrutinee"
  let genAlts expr = do -- collect (result,block) pairs to emit the phi instr
         b <- block
         r <- stgToIR expr
         br endBlock
         return (r, b)
  switchAlts <- return $ zip vals (map snd retBlockPairs)
  switch scrut dBlock switchAlts
  retBlockPairs <- mapM genAlts altCode
  dBlock <- block -- `named` "default_branch"
  dSsa <- case defaultBranch of
      Just code -> stgToIR code <* br endBlock
      Nothing   -> do
          errFn <- gets ((\(Just (ContFn f)) -> f) . (Map.!? "NoPatternMatchError") . bindMap)
          call errFn [] <* unreachable
  endBlock <- block -- `named` "endBlock"
  let phiBlocks = case defaultBranch of -- skip the 'void' default branch.
          Just _ -> (dSsa, dBlock) : retBlockPairs
          Nothing -> retBlockPairs
  phi phiBlocks

-- ********
-- * Data *
-- ********
-- Data case deconstruction. use deconProduct or deconSum from DataToLLVM.hs
-- maybeDefault is only used in sumtype deconstructions
stgToIR (StgDataCase scrut _ []) = error "cannot deconstruct an empty sum type"
stgToIR (StgDataCase scrut maybeDefault alts) =
  stgToIR scrut >>= \dataStruct ->
  case alts of
     [p@(iden, membs, e)] -> deconProduct dataStruct p >>= stgToIR . flip StgLet e
     alts -> stgToIR =<< deconSum dataStruct maybeDefault alts

-- StgApp for constructors: we're responsible for memory returned to this stack frame
-- So if we don't return it here, it needs to be freed.
stgConApp :: CodeGenIRConstraints m
  => StgId -> [StgArg] -> m Operand
stgConApp iden baseArgs =
  gets ((Map.!? iden) . dataFnsMap) >>= \case
    Nothing -> error (show iden ++ ": unbound function")
    Just (ProxyDataFn sz fn) ->          handleMemParam sz fn baseArgs
    Just (DataFns sz con membs destr) -> handleMemParam sz con baseArgs
    
    -- Data lifespan: Either 
    -- * this stack frame returns it, -> use mem arg that was passed to this fn
    -- * it expires on this stack frame -> alloca (and maybe queue destructor)
    -- * it is subdata -> malloc (a destructor will clean this later)
  where
   handleMemParam :: CodeGenIRConstraints m
     => StgConst -> StgFn -> [StgArg] -> m Operand
   handleMemParam sz fn args = gets subData >>= \case
    True -> do
        mem <- gets ((\(Just (ContFn m))->m) . (Map.!? "malloc") . bindMap) >>=
          \malloc -> call malloc [(ConstantOperand sz, [])]
        params <- map (,[]) <$> mapM (stgToIR . StgLit) args
        call fn ((mem,[]) : params)

    False -> gets stackFrame >>= \case
      -- No active stack frame -> this frame is responsible for this data
      Nothing -> do
          -- traceM "no active frame"
          modify (\x->x{ subData = True })
          mem <- alloca (IntegerType 8) (Just $ ConstantOperand sz) 0
          params <- map (,[]) <$> mapM (stgToIR . StgLit) args
          call fn ((mem,[]) : params)

      -- Use the current fns mem arg
      Just sf@(StackFrame stackSz memPtr destrQueue) -> do -- top data: give it stack mem
          -- traceM "active frame"
          -- eval params using the new stackframe state
          modify (\x->x{ subData = True })
          modify (\x->x{ stackFrame = Nothing })
          params <- map (,[]) <$> mapM (stgToIR . StgLit) args
          modify (\x->x{ stackFrame = Just sf })

          -- call the constructor with our stack memory
          let memArg = (memPtr, [Returned, NonNull])
          call fn (memArg : params)
