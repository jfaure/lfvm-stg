{-# LANGUAGE StandaloneDeriving, LambdaCase #-}
-- LFVM STG is a very thin layer over LLVM (Using LLVM Types and Instructions),
-- See StgToLLVM for a detailed description of how this maps to llvm
--
-- LFVM is an STG (Spineless tagless g-machine)
-- Spineless: no single data structure: top level bindings reference each other
-- Tagless: Heap values contain no annotations (like type/evaluated already?)..
-- Graph-reducing: closures can be overwritten by simpler values.
--
-- 1. LFVM STG only understands LLVM types, and algebraic sum/products
-- 2. All free vars become explicit arguments before codegen.
--   "Free vars" are functions (arity >=0) residing in llvm global scope.
-- 3. Data constructors are desugared to tagged StgCases beforehand
--    "trivial" types become unions/structs, but
--     sum types that refer to themselves become closures.
--     TODO: we could (?!) optimize some to dynamic/static arrays

module StgSyn where

import qualified LLVM.AST (Operand, Instruction, Type, Name)
import qualified LLVM.AST.Constant (Constant)
import Data.ByteString.Char8 (ByteString)

-- Leaf types
-- [StgOps]: some llvm Instructions have flags, but stg only cares about operands.
type StgConst = LLVM.AST.Constant.Constant
type StgSsa   = LLVM.AST.Operand
type StgId    = ByteString
type StgUnOp  = LLVM.AST.Operand -> LLVM.AST.Instruction
type StgBinOp = LLVM.AST.Operand -> LLVM.AST.Operand -> LLVM.AST.Instruction

-- a StgVarArg is a named SSA or closure (0 arity function)
-- somthing that trivially resolves to an ssa passable to an llvm function
-- Note. this includes function pointers!
data StgArg
  = StgLitArg  StgConst -- constant value
  -- | StgSsaArg  StgSsa   -- used internally (actually unused atm)
  | StgVarArg  StgId    -- key for bindMap
  | StgExprArg StgExpr  -- full expr

-- let bindings. Closures = Rhs of a binding (functions / thunks)
data StgBinding = StgBinding StgId StgRhs
data ClosureUpdateFlag = Updateable | SingleEntry | ReEntrant
data StgRhs
  = StgRhsClosure !ClosureUpdateFlag -- Updateable | SingleEntry | ReEntrant
                  [StgArg]           -- The type: Can be empty if this is a var.
                  [LLVM.AST.Type]    -- Arg Types
                  LLVM.AST.Type      -- a closure returns a primitive
                  StgExpr
  | StgRhsSsa     StgSsa             -- used esp. for locally binding function params.
  | StgRhsCon     DataCon [StgArg]

-- Primitive operations: llvm instructions and foreign calls
data StgPrimOp
 = StgPrimUnOp StgUnOp StgExpr
 | StgPrimBinOp StgBinOp StgExpr StgExpr
-- = StgForeign StgId [StgArg] -- args must be saturated

data StgExpr
  = StgLit StgArg

  | StgPrimOp  StgPrimOp -- primitive operations

  | StgApp  StgId     --function
            [StgArg]  --args (maybe empty)

  | StgLet          [StgBinding]
                    StgExpr

  | StgCaseSwitch   StgExpr               -- scrutinee
                    StgExpr               -- default. (often error >> exit)
                    [(LLVM.AST.Constant.Constant, StgExpr)] -- values -> Alternatives

  -- DCon stands for 'DeConstructor', semantically exactly a constructor.
  -- Note this will probably be removed since it isn't part of the codegen stg.
  | StgCaseDCon     StgExpr               -- scrutinee
                    [(DataCon, StgExpr)]  -- constructors -> Alternatives

-- fmap over all stgExprs referenced by an stgExpr
-- This would be a functor instance if StgExpr had kind *->*
-- As it stands, it's a bit of a strange function, but very useful.
stgMap :: (StgExpr -> StgExpr) -> StgExpr -> StgExpr
stgMap f e = let go e = stgMap f $ f e
  in f $ case e of
  StgPrimOp (StgPrimUnOp op e) -> (StgPrimOp (StgPrimUnOp op (go e)))
  StgPrimOp (StgPrimBinOp op e e2) -> StgPrimOp (StgPrimBinOp op (go e) (go e2))
  StgApp i arglist -> StgApp i $ -- some args are full stgExprs - find them.
       ((\case { StgExprArg e -> StgExprArg $ go e ; arg -> arg }) <$> arglist)
  StgLet binds e2 -> StgLet binds (go e2)
  StgCaseSwitch e2 e3 l -> StgCaseSwitch (go e2) (go e3) l'
      where l' = (\(c, e) -> (c, go e)) <$> l
  simpleExpr -> f simpleExpr

-- DataCon
-- STG must understand sum types (unions in llvm)
-- product types are the llvm native type struct,
-- stg must handle this personally so it can recognize fields.
--
-- trivial sum types are unions,
-- sum types that refer to themselves are much tricker to handle. (eg. [])
-- In the general case, they have to be closures.
data DataCon
  = Unboxed        LLVM.AST.Type
  | ProductTypeCon [DataCon]
  | SumTypeCon     [DataCon]

instance Show StgPrimOp
  where show (StgPrimBinOp f a b) = "(" ++ show a ++ ") (op) (" ++ show b ++ ")"
        show (StgPrimUnOp f a) = "(op) (" ++ show a ++ ")"
deriving instance Show StgExpr
deriving instance Show DataCon
deriving instance Show StgRhs
deriving instance Show StgBinding
deriving instance Show StgArg
deriving instance Show ClosureUpdateFlag
