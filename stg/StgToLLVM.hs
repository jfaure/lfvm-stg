{-# LANGUAGE
  FlexibleContexts, MultiWayIf, LambdaCase, RecursiveDo, OverloadedStrings,
  ConstraintKinds, NoMonomorphismRestriction, RankNTypes, TupleSections
#-}
-- {-# OPTIONS -Wall #-}
module StgToLLVM -- (stgToIRTop) -- :: [STGTopBinding] -> LLVM.Module
where

-- Local imports
import StgSyn
import LlvmHsExts (globalStringPtr, constZero, charPtrType, unknownPtrType, sizeof)

-- LLVM
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction hiding (globalStringPtr)

import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Typed
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as P
import qualified LLVM.AST.FloatingPointPredicate as Pf
import LLVM.AST.Global
import LLVM.AST.Linkage
import LLVM.AST.Typed
import LLVM.AST.AddrSpace
import LLVM.AST.Attribute

import Data.Char
import Data.String (fromString)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Fix (MonadFix)
import Data.Functor.Identity (Identity)
--import Data.Vector (Vector, empty, (!), snoc)
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
-- 
-- Applications 
-- LLVM functions must be saturated when called.
-- Partial applications are trivial, we just pass a function pointer as a normal argument.
--
-- Case expressions and data constructors
-- value cases are llvm switch instructions
-- Data declarations spawn some llvm functions: con, decon, destruct (if recursive data)

-- **************
-- * LLVM notes *
-- **************
-- We can mark most llvm functions as 'pure', which is helpful to llvm optimizations
-- useful LLVM optimization passes:
-- -reassociate       -aims to improve constant propagation
-- -block-placement   -rearrange switch blocks in case expressions
-- -simplifycfg
-- - ?                -merge gep instructions in nested structs
-- -inline
-- -jump-threading    -branch analysis
-- -tailcallelim
--
-- ******************************************
-- * Semantics of code gen :: STG -> LLVMIR *
-- ******************************************
-- codegen happens within llvm-hs-pure ModuleBuilder and IRBuilder monads.
-- StgCase uses mdo (MonadFix) to generate phi blocks
-- stgToIR also relies on a MonadState StgToIRState

type CodeGenModuleConstraints m = (MonadState StgToIRState m,
                                   MonadModuleBuilder m,
                                   MonadFix m)
type CodeGenIRConstraints m = (CodeGenModuleConstraints m,
                               MonadIRBuilder m)

-- unfortunately ghc doesn't handle impredictive polymorphism yet,
-- so we can't store this function type in the stgToIRState:
-- type DeConFnType = forall (m :: * -> *).
--   (MonadIRBuilder m, MonadModuleBuilder m, MonadState StgToIRState m, MonadFix m)
--   => [StgId] -> Operand -> StgExpr -> Maybe [StgExpr]
--  As a result we have to seperate packing/unpacking logic for data.. :(

-- Memory management:
-- Stack frames are responsible for all data returned by functions they call !
-- Each stack frame has a {i8*, [n x i8*]}: a ptr to prev stack frame + alloca'd memory
-- The stack size needed is static and is sort of calculated anyway during codegen:
-- we only need to add an accumulator (stackMemSize) in the StgToIRState
-- Recursive/lazy data must in general be on the heap:
-- stack frames are as before responsible for them and must call their destructors.
-- alloca memory obviously expires automatically as a function returns

-- Complications occur when we try to inline sub-data,
-- since the sub-data might outlive the parent data,
-- in which case it has to be stored on an earlier stack frame than the parent

-- Note. the alloca size is part of the type: but it can be bitcasted to a smaller array
-- as functions take some of its memory.
-- Data functions need to alloca static memory needed by callees
-- each stack frame for a data function contains an additional pointer to the previous stack frame
-- which may be used to write data to depending on each datas lifetime
stackFrameStructType nBytes =
  StructureType { isPacked = False
                , elementTypes = [charPtrType, ArrayType nBytes charPtrType]
                }

-- ****************
-- * StgToIRState *
-- ****************
data Symbol
  = ContLi LLVM.AST.Operand  -- an SSA - can be passed through llvm functions.
  | ContFn LLVM.AST.Operand  -- a function. (Note. llvm calls are alway saturated)
  | ContRhs StgRhs           -- Placeholder (we can let-bind this immediately)

type TypeMap    = Map.Map StgId StgType
type SymMap     = Map.Map StgId Symbol
type DataFnsMap = Map.Map StgId DataFns

-- StackMem: this record is only used while codegenning functions returning data
-- Idea is to provide them with memory on the last stack frame that uses the data
-- recursive data is on the heap, so the last stack frame must also call deconstructors
data StackFrame = StackFrame
  { stackMemSize  :: C.Constant -- How many (alloca) bytes needed by fns called from here.
  , stackMem      :: StgSsa     -- Pointer to free memory on a previous stack frame
  , prevFrame     :: StgSsa     -- pointer to prev stack frame (StackFrameStructType),
  }                             -- can reach all prev frames using this

-- Data Funs: Con, ~Con
data DataFns = DataFns -- llvm fns generated when data is defined
  { staticSize:: C.Constant  -- normally alloca memory: sizeof data (not incl. subdata)
  , construct :: StgFn       -- llvm packing fn (fills the struct / create thunk(s))
--, decon     :: [StgFn]     -- fn to extract each member (llvm can easily inline)
  , destruct  :: Maybe StgFn -- recursive data must recursively free
  }
  | DataConst LLVM.AST.Operand -- (ConstantOperand global)

-- note. constructors are stored in dataFnsMap but also in bindMap for convenience (?!)
data StgToIRState = StgToIRState 
  { bindMap       :: SymMap          -- llvm things available (at a given stack frame)
  , dataFnsMap    :: DataFnsMap      -- it's cleaner to seperate this from the symMap
  , typeMap       :: TypeMap         -- Type aliases (probably should deprecate) 
  , stackFrame    :: Maybe StackFrame -- for fns returning data
  }

-- stgToIRTop:  This module's only export
stgToIRTop :: [StgTopBinding] -> LLVM.AST.Module
stgToIRTop topBindings =
   let stgToIRState = StgToIRState
           { bindMap      = Map.empty
           , dataFnsMap   = Map.empty
           , typeMap      = Map.empty
           , stackFrame   = Nothing
           }
       -- necessary for handling recursive algebraic data
       rtsDecls = [("malloc", [IntegerType 32], charPtrType)
                  ,("free",   [charPtrType],    VoidType)
                  ,("error",  [IntegerType 32, IntegerType 32, charPtrType], VoidType)
                  ]
       emitCustomRtsFns = do
          let z = ConstantOperand $ C.Int 32 0
              noMatchFnName = "NoPatternMatchError"
              getPrevAllocaFName = "getPrevStackFrame" -- TODO
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
         in mapM go rtsDecls
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

-- Resolves type aliases (also check for alias loops)
aliasToType :: StgId -> TypeMap -> Maybe StgType
aliasToType iden map = go iden map []
  where go iden map aliases = (Map.!? iden) map >>= \case -- Maybe monad
            StgTypeAlias nm -> if iden `elem` aliases
                               then Nothing
                               else go iden map (iden : aliases)
            any             -> return any

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

-- getType: get an llvm type from an StgType
getType :: CodeGenModuleConstraints m => StgType -> m LLVM.AST.Type
getType (StgLlvmType t) = return t
getType (StgTyperef t t2) = return t
getType (StgTypeAlias iden) = maybe (error "null typedef") getType =<<
                                 gets (aliasToType iden . typeMap)
-- used only by gen constructors
getType (StgAlgType stgData@(StgSumType iden alts)) = do
  conInfo <- gets (aliasToType iden . typeMap)
  case conInfo of
    Just (StgLlvmType ty) -> return ty
    Nothing -> genConstructor stgData >> getType (StgAlgType (StgSumType iden alts))
-- used only by fnToLlvm
getType (StgFnType types) = do
    llvmTypes <- mapM getType types
    let (argTypes, [retType]) = splitAt (length llvmTypes - 1) llvmTypes
        isVarArg = False
        fnType = FunctionType retType argTypes isVarArg
        fnPtr = PointerType fnType (AddrSpace 0)
    return fnPtr

-- fnToLlvm: Find the type of the function, emit it and hand over to stgToIR for the body
-- This is relatively straightforwards, just remember that functions returning data
-- need an extra parameter: a pointer to stack memory they can use,
-- and functions calling these need to pass stack memory (from their frame or a prev one !)
fnToLlvm :: (CodeGenModuleConstraints m)
  => StgId -> StgRhs -> m Operand
fnToLlvm iden (StgRhsClosure args argTypes retType body) = mdo
    retTy <- getType retType
    params' <- map (, ParameterName "a") <$> mapM getType argTypes
    let params = params'
--  let params = case retTy of -- fns handling data also need a pointer to free memory
--          StgAlgType{} -> CharStar : params'
--          notData      -> params'
    --argLlvmTys <- mapM getTy argTypes
    --params <- zipWithM (\ty name -> (ty, ParameterName name)) argLlvmTys args
    modify (\x->x { bindMap = Map.insert iden (ContFn f) (bindMap x) })
    f <- function iden params retTy $ \llvmArgs ->
        -- fns bodies handled as: let args in fn
        let argBinds = zipWith StgBinding ((\(StgVarArg a) -> a) <$> args) -- ! litargs
                                          (StgRhsSsa <$> llvmArgs)
        in do -- sv State, codeGen a let, then clear the argument bindings
        svState <- get -- function args may squash bindings, so save and restore.
        ret =<< stgToIR (StgLet argBinds body)
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
-- One might assume this is trivial, except we must emit constant arrays as globals
-- and call 0 arity functions (thunks)
stgToIR (StgLit lit) =
  let handleArray :: CodeGenIRConstraints m => C.Constant -> m C.Constant
      handleArray arr@C.Array{} = -- const arrays need to be emmited in global scope
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
             let zz = [C.Int 32 0, C.Int 32 0]
             return $ C.GetElementPtr True (C.GlobalReference (ptr ty) nm) zz
      handleArray notArray = return notArray

      -- some literals are in fact 0 arity functions that must be resolved.
      callIfThunk :: CodeGenIRConstraints m => Operand -> m Operand
      callIfThunk s = case LLVM.AST.Typed.typeOf s of
          FunctionType _ [] _                 -> call s []
          PointerType (FunctionType _ [] _) _ -> call s []
          dont_call                          -> return s

      handle0Arity :: CodeGenIRConstraints m => StgId -> Symbol -> m Operand
      handle0Arity iden = \case
          ContLi s  -> return s --  never call literals
          ContFn v  -> callIfThunk v
          ContRhs (StgRhsSsa val) -> return val -- don't make a function for literals
          ContRhs r -> fnToLlvm iden r >>= callIfThunk -- rly
  in case lit of
     StgLitArg l ->  ConstantOperand <$> handleArray l
     StgSsaArg s ->  return s
     -- some varArgs are constructors or lazy closures that we must resolve (call) now.
     StgVarArg i ->  gets ((Map.!? i) . bindMap) >>= \case
                         Just v -> handle0Arity i v
                         Nothing -> error (show i ++ ": variable not found in bindMap")
     StgExprArg e -> stgToIR e

-- PrimOps = extract raw values and emit the primop's llvm instruction.
-- Might spawn a thread if at least one side is a function application
stgToIR (StgPrimOp (StgPrimBinOp op a b)) = do
  a' <- stgToIR a
  b' <- stgToIR b
  emitInstr (typeOf a') $ op a' b'

-- Arities always exactly match
-- Recall stack frames are responsible for all data returned by their functions !
-- We may need to alloca some stack space and pass that to the function
stgToIR (StgApp iden args) = do -- function application
  -- prepare arguments.
  ssaArgs <- mapM (stgToIR . StgLit) args
  let params = (,[]) <$> ssaArgs -- the [] = parameter attributes
  -- Get the function. It might not yet be codegened, but should exist in our bindMap.
  -- Try the bindMap (regular/vanilla llvm fns)
  gets ((Map.!? iden) . bindMap) >>= \case
     Just x -> (`call` params) =<< case x of
       ContLi f -> error "StgToIR ($): panic: unexpected literal"
       ContFn f -> return f
       ContRhs (StgRhsSsa val) -> return val -- a function pointer arg for us to call
       ContRhs rhs@StgRhsClosure{} -> fnToLlvm iden rhs
     -- Try the Data map
     Nothing -> gets ((Map.!? iden) . dataFnsMap) >>= \case
       Nothing -> error (show iden ++ ": function not found in bindMap")
       -- It is data: we need to find the right stack memory to give it
       Just (DataFns sz con destr) -> do
          mem <- gets ((\(Just (ContFn m))->m) . (Map.!? "malloc") . bindMap) >>=
            \malloc -> call malloc [(ConstantOperand sz, [])]
          call con ((mem,[]) : params)
--        mem <- alloca (C.Int 32 sz)
--        let inAlloca = (mem, [InAlloca, Returned, NonNull])
--        call f (inAlloca : params)
--        -- maybe free some stuff ?

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
-- Data case deconstruction =~ let-bind deconArgs in expr
stgToIR (StgDataCase scrut maybeDefault alts) = do
  dataStruct <- stgToIR scrut
  -- Product decon: see genProductConstructor
  let deconProduct (iden, args, e) =
       let nMembers = fromIntegral $ length args
           gepArg idx = ConstantOperand . C.Int 32 <$> [0, idx]
           idxToVal idx = gep dataStruct (gepArg idx) >>= \ptr -> load ptr 0
       in
       mapM idxToVal [0..nMembers - 1] >>= \ssaMembers ->
       let mkBinding name ssa = StgBinding name (StgRhsSsa ssa)
           bindings = zipWith mkBinding args ssaMembers
       in case maybeDefault of
           Just a -> stgToIR $ StgLet bindings a
           Nothing -> case alts of -- error "Product decon: expected expr"
               [_] -> stgToIR $ StgLet bindings e
               _ -> error "Product type should have exactly 1 'sumtype alt'"

  -- Sum deconstruction: see genSumConstructor
  case alts of
      [] -> error "cannot deconstruct an empty sum type (?!)"
      [productDecon] -> deconProduct productDecon
      -- Sum decon: see genSumConstructor. dispatch a switch on the tag
      alts -> do
          let nAlts = fromIntegral $ length alts
              zero = ConstantOperand $ C.Int 32 0
              one  = ConstantOperand $ C.Int 32 1
          tag <- gep dataStruct [zero, zero] >>= \ptr -> load ptr 0 `named` "tag"
          valPtr <- gep dataStruct [zero, one] `named` "valPtr"
          let mkProduct tagNumber subProductType@(iden, fieldNames, expr) =
                gets (aliasToType iden . typeMap) >>= \case
                  Nothing -> error "no deconstructor"
                  Just (StgTyperef ty subTy) -> do
                      let valPtrType = PointerType subTy (AddrSpace 0)
                      castStruct <- bitcast valPtr valPtrType `named` "cast"
                      val <- load castStruct 0
                      return (C.Int 32 $ fromIntegral tagNumber,
                             -- bitcast to data # tagNumber !
                              StgDataCase (StgLit (StgSsaArg val))
                                          (Just expr)
                                          [subProductType]
                              )
                  _ -> error "Panic: No subtype" -- ?!
          taggedProducts <- zipWithM mkProduct [0..nAlts - 1] alts
          stgToIR (StgCaseSwitch (StgLit $ StgSsaArg tag) Nothing taggedProducts)

-- **********************
-- * DataFns generation *
-- **********************
-- genConstructor
-- In general: productTypes -> struct ; sumTypes -> tag + void* pointer
-- Data is written to the (usually inAlloca) arg given.
-- Recursive data calls malloc
genConstructor :: CodeGenModuleConstraints m  => StgData -> m Operand
genConstructor = genSumConstructor

-- Product types need to call the sum constructor to become 'embedded' in the sum type
-- embedded product types have the same type as the outer sum constructor.
data InSumType = InSumType
 { sumConFn  :: LLVM.AST.Operand -- sumtype Constructor Function
 , tagNumber :: LLVM.AST.Operand -- tag number for this product type
 , sumType   :: LLVM.AST.Type    -- return type (of sumConFn, since we must tail call that)
 }

-- Generate the constructor function
genProductConstructor :: CodeGenModuleConstraints m 
  => StgProductType -> Maybe InSumType -> m Operand
genProductConstructor algData@(StgProductType name fields) sumConstructor = do
  types <- mapM getType fields
  let productType = StructureType { isPacked = False, elementTypes = types }
      nMembers = fromIntegral $ length types
      structName = name
      params = map (, ParameterName "A") types
      conFnName = name

  -- make a new typedef or use the sumtype if we're in one
  structType <- case sumConstructor of
      Nothing -> typedef name (Just productType)
      Just (InSumType conFn tag sumType) -> return sumType
  let retType = PointerType structType (AddrSpace 0)
      mainType = retType -- PointerType retType (AddrSpace 0)
      subType  = PointerType productType (AddrSpace 0)
      deCon = StgTyperef mainType subType
  modify (\x->x { typeMap = Map.insert name deCon (typeMap x) })

  -- generate llvm constructor function (or global constant if it takes no params)
  -- if empty product type (within a sum type) -> make a global constant, not a function.
  let genCon [] = case sumConstructor of
          Nothing -> error "empty product type" -- Only legal within sumtypes, eg. `Nothing`
          Just (InSumType conFn tag sumType) -> do
             let voidPtrTy = PointerType (IntegerType 8) (AddrSpace 0)
                 sumType = (\(PointerType t _) -> t) retType
                 sval = C.Struct Nothing False [C.Int 32 0, C.Null voidPtrTy]
                 val  = C.BitCast sval sumType -- necessary to force a typematch
             g <- global structName sumType val -- llvm complains if we use the typedef..
             modify (\x->x { bindMap = Map.insert name (ContLi g) (bindMap x) })
             return g

      genCon _ = -- generate constructor function (always starts with memory in which to write)
        let sz = sizeof retType
            fnBody (mem : conArgs) = do
              let sz = sizeof structType
              structPtr <- bitcast mem retType
              let fillProduct prodPtr = zipWithM_ storeVal conArgs [0..]
                   where storeVal ssa idx = gep prodPtr [constZero, ConstantOperand $ C.Int 32 idx]
                           >>= \ptr -> store ptr 0 ssa
              case sumConstructor of
                  Nothing -> fillProduct structPtr >> ret structPtr
                  Just (InSumType conFn tag sumType) -> do
                     voidPtr <- bitcast structPtr (PointerType (IntegerType 8) (AddrSpace 0))
                     prodMem <- gets ((\(Just (ContFn m))->m) . (Map.!? "malloc") . bindMap) >>=
                        \malloc -> call malloc [(ConstantOperand sz, [])]
                     prodPtr <- bitcast prodMem subType
                     fillProduct prodPtr
                     voidPtr <- bitcast prodPtr charPtrType
                     ret =<< call conFn [(mem, []), (tag,[]), (voidPtr,[])]
        in function conFnName ((charPtrType,"mem") : params) retType fnBody >>= \conFn ->
           modify (\x->x { dataFnsMap = Map.insert name (DataFns sz conFn Nothing) (dataFnsMap x) })
           >> return conFn
  genCon params

-- Sum types become a struct containing a tag and void* pointer
-- the pointer is bitcasted based on the tag
genSumConstructor :: CodeGenModuleConstraints m  => StgSumType -> m Operand

-- Only one alt = this sum type is actually a product type
genSumConstructor (StgSumType name [productType]) = genProductConstructor productType Nothing

genSumConstructor algData@(StgSumType name productTypes) = do
  -- Start by defining the sumtype as {i32, i8*} (tag and pointer)
  -- emit a typedef for typesafety
  let voidPtrType = PointerType (IntegerType 8) (AddrSpace 0)
      tagType = IntegerType 32
      structName = name ; conFnName = name
      nMembers = 2
      structType = StructureType packed [tagType, voidPtrType] where packed = False
      sz = sizeof structType
      params = [(tagType, ParameterName "tag"), (voidPtrType, ParameterName "unionPtr")]
  structType <- typedef structName (Just structType)
  let structPtrType = PointerType structType (AddrSpace 0)
  modify (\x->x { typeMap = Map.insert name (StgLlvmType structPtrType) (typeMap x) })

  -- generate llvm constructor function
  -- This is just a wrapper (it sets the tag) for the sub types contained within to call
  conFn <- function conFnName ((charPtrType,"Mem") : params) structPtrType $
   \[mem, tag, valVoidPtr] -> do
      sumTyPtr <- bitcast mem structPtrType `named` "return-mem"
      tagPtr <- gep sumTyPtr [constZero, constZero]
      store tagPtr 0 tag
      unionValPtr <- gep sumTyPtr [constZero, ConstantOperand $ C.Int 32 1]
      store unionValPtr 0 valVoidPtr
      ret sumTyPtr -- return this no matter what.
  modify (\x->x { dataFnsMap = Map.insert name (DataFns sz conFn Nothing) (dataFnsMap x) })

  let mkSubType (StgProductType deConId structTy) tag = do
        conInfo <- gets (aliasToType deConId . typeMap)
        case conInfo of
          Just ty -> error "Constructor already exists with that name"
          Nothing -> let product = StgProductType deConId structTy
                         tag' = ConstantOperand $ C.Int 32 $ fromInteger tag
                     in genProductConstructor product $ Just (InSumType conFn tag' structType)
  zipWithM_ mkSubType productTypes [0..] -- codegen all this sumtypes constructors
  return conFn
