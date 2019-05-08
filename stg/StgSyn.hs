{-# LANGUAGE StandaloneDeriving #-}
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

-- type StgModule = [StgTopBinding]
-- let bindings: data def | function def | extern decl
-- Data definitions: These require 2 functions:
--   1. llvm constructor function
--   2. a codeGen decon function that resolves sumType tags and
--      unpacks (LLVM gep + letBind) struct members
data StgTopBinding
 = StgTopBind    StgBinding -- constant | function def | extern decl
 | StgTopData    StgData    -- data = sumType = [ProductTypes]
 | StgTopTypeDef StgId StgType

-- **************
-- * Leaf types *
-- **************
-- [StgOps]: some llvm Instructions have flags, but stg only cares about operands.
type StgConst = LLVM.AST.Constant.Constant
type StgSsa   = LLVM.AST.Operand
type StgId    = LLVM.AST.Name
type StgUnOp  = LLVM.AST.Operand -> LLVM.AST.Instruction
type StgBinOp = LLVM.AST.Operand -> LLVM.AST.Operand -> LLVM.AST.Instruction

data StgType  -- All these must eventually become llvm equivalents
 = StgLlvmType  LLVM.AST.Type -- Vanilla type (could still be a struct)
 | StgTypeAlias StgId         -- resolved using typeMap during codegen
 | StgFnType    [StgType]     -- stg function type (not extern)
 | StgAlgType   StgData       -- By far the trickiest to handle and optimize
-- StgTypeRef: Internally we store alias+type for StgAlgTypes
-- aliases produce clearer llvm but we also need the initial type (if its a fn/struct)
 | StgTyperef   LLVM.AST.Type LLVM.AST.Type

-- StgArg = resolves (stgToIR) to an ssa passable to an llvm function
-- Note. this includes function pointers!
data StgArg
  = StgLitArg  StgConst -- constant value
  | StgSsaArg  StgSsa   -- used only by union tags in case atm.
  | StgVarArg  StgId    -- key for bindMap (for a named SSA or 0 arity function)
  | StgExprArg StgExpr  -- full expr

-- ************
-- * Bindings *
-- ************
-- All data is modeled as a sum type: they may have 0 alts or 0 fields.
-- it's a natural way to handle things like `data E = A _ _ | B _ | C`
-- The usual convention is that complex types contain pointers to their children
-- Some data can and should be buffered (ie. not a naive heap allocated linked list)
-- nullptr children indicate lazy evaluation
-- the sumType tag is a 1:1 mapping to their alts, and is used to mark leaves.
data StgProductType = StgProductType StgId [StgType]
data StgSumType     = StgSumType     StgId [StgProductType]
type StgData        = StgSumType

data StgBinding = StgBinding StgId StgRhs
data StgRhs
  -- function definition
  = StgRhsClosure [StgArg]           -- Arg Names
                  [StgType]          -- Arg Types
                  StgType            -- return Type
                  StgExpr            -- Function body
  | StgExtern     [StgType] StgType -- external fn for use in StgApp
  | StgExternVa   [StgType] StgType -- extern varArgs function (eg. printf)
  | StgRhsConst   StgConst -- =~ globals
  | StgRhsSsa     StgSsa   -- used internally for local bindings (esp.function arguments)

data StgPrimOp
 = StgPrimUnOp  StgUnOp  StgExpr
 | StgPrimBinOp StgBinOp StgExpr StgExpr

data StgExpr
  = StgLit     StgArg

  | StgPrimOp  StgPrimOp -- primitive operations

  | StgApp     StgId     --function name
               [StgArg]  --args (maybe empty)

  | StgLet     [StgBinding]
               StgExpr

  -- Switch on primitive values
  | StgCaseSwitch   StgExpr               -- scrutinee
                    (Maybe StgExpr)       -- default. (often error >> exit)
                    [(LLVM.AST.Constant.Constant, StgExpr)] -- values -> Alternatives

  -- Type deconstructors
  -- note. data is always a sumtype of product types (1 alt = product type)
  -- ProductType: let structMembers in expr
  -- SumType:     switch on a tagged union
  | StgDataCase     StgExpr   -- a struct (or union = a struct {tag,voidPtr})
                    (Maybe StgExpr) -- default branch (or 'in' expr if product type)
                    [(StgId, [StgId], StgExpr)] -- (Decon [args] -> dispatchExprs)

-- Cheap show instances
instance Show StgPrimOp
  where show (StgPrimBinOp f a b) = "(" ++ show a ++ ") (op) (" ++ show b ++ ")"
        show (StgPrimUnOp f a) = "(op) (" ++ show a ++ ")"
deriving instance Show StgExpr
deriving instance Show StgSumType
deriving instance Show StgProductType
deriving instance Show StgType
instance Show StgRhs
  where show (StgRhsClosure args tys retty e) = show args ++ show e
        show (StgRhsSsa s) = "closure: " ++ show s
        show (StgExtern l r) = "extern"
        show (StgExternVa l r) = "externVa"
        show (StgRhsConst l) = show l
instance Show StgBinding
  where show (StgBinding id rhs) = show id ++ " = " ++ show rhs ++ "\n"
deriving instance Show StgArg
instance Show StgTopBinding
  where show (StgTopBind b) = show b ++ "\n\n"
        show (StgTopData d) = show d ++ "\n\n"
        show (StgTopTypeDef nm ty) = "type " ++ show nm ++ " = " ++ show ty
