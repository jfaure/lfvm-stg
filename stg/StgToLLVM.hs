{-# LANGUAGE
  FlexibleContexts, MultiWayIf, LambdaCase, RecursiveDo, OverloadedStrings
, ConstraintKinds, NoMonomorphismRestriction, RankNTypes, KindSignatures
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
import qualified LLVM.AST.Typed as T
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as P
import qualified LLVM.AST.FloatingPointPredicate as Pf
import LLVM.AST.Global
import LLVM.AST.Typed
import LLVM.AST.Linkage
import LLVM.AST.Typed
import LLVM.AST.AddrSpace
import Data.Char
import Data.String (fromString)

--import Data.Vector (Vector, empty, (!), snoc)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Fix (MonadFix)
import Data.Functor.Identity (Identity)

-- It would be preferable to externalize functions that must operate on LLVM.AST.Name
import Data.ByteString.Short (toShort, pack, unpack, ShortByteString)
import qualified Data.ByteString.Internal as BS (c2w) -- Char -> Word8

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
-- Only functions are legal free variables in llvm, they reside in module scope
-- The module conatains a closure (function) for all bindings,
-- with the exception of local constants.
-- 
-- Applications 
-- LLVM functions must be saturated when called.
-- we use the push-enter model: the caller handles function arity.
-- (as opposed to eval-apply, where functions are responsible for their arities)
--
-- Case expressions and data constructors
-- value cases are llvm switch instructions
-- Data declarations spawn 2 functions
-- 1. llvm function that packs arguments in a struct
-- 2. DeConFn: codeGen helper that unpacks and let-binds members (when case deconstructed)

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

-- StgToIRState
-- symbols stored in the codegen bindMap
data Symbol
  = ContLi LLVM.AST.Operand  -- an SSA - can be passed through llvm functions.
  | ContFn LLVM.AST.Operand  -- a function. (Note. llvm calls are alway saturated)
  | ContRhs StgRhs           -- Placeholder (we can let-bind this immediately)

-- unfortunately ghc doesn't handle impredictive polymorphism yet,
-- so we can't store this function type in the stgToIRState:
-- type DeConFnType = forall (m :: * -> *).
--  (MonadIRBuilder m, MonadModuleBuilder m, MonadState StgToIRState m, MonadFix m)
--  => [StgId] -> Operand -> StgExpr -> Maybe [StgExpr]
--  As a result we have to seperate packing/unpacking logic for data.. :(

-- sum types constructors carry 2 types during codegem:
-- the main type + subtype: eg. in List a = cons a | Nothing, cons has types list and a
-- (ty, subTy, stgDecl)

type TypeMap = Map.Map StgId StgType
type SymMap  = Map.Map StgId Symbol
data StgToIRState = StgToIRState 
  { bindMap  :: SymMap  -- llvm things available (at a given stack frame)
  , typeMap  :: TypeMap -- Deconstructor functions.
  }

-- Resolves type aliases (also check for alias loops)
aliasToType :: StgId -> TypeMap -> Maybe StgType
aliasToType iden map = go iden map []
  where go iden map aliases = (Map.!? iden) map >>= \case -- Maybe monad
            StgTypeAlias nm -> if iden `elem` aliases
                               then Nothing
                               else go iden map (iden : aliases)
            any             -> return any

-- stgToIRTop:  This module's only export
stgToIRTop :: [StgTopBinding] -> LLVM.AST.Module
stgToIRTop topBindings =
   let stgToIRState = StgToIRState
           { bindMap = Map.empty
           , typeMap = Map.empty
           }
       -- necessary for handling recursive algebraic data
       rtsDecls = [("malloc", [IntegerType 32], charPtrType)
                  ,("free",   [charPtrType],    VoidType)
                  ,("error",  [IntegerType 32, IntegerType 32, charPtrType], VoidType)
                  ]
       emitRtsConsts = do
          let z = ConstantOperand $ C.Int 32 0
              fname = "NoPatternMatchError"
          failStr <- globalStringPtr "No default switchCase alt" "noPatternMsg"
          f <- function fname [] VoidType $ \[] -> do
            errFn <- gets ((\(Just (ContFn f)) -> f) . (Map.!? "error") . bindMap)
            call errFn [(z,[]), (z,[]), (ConstantOperand $ failStr,[])]
            unreachable
          modify (\x->x{ bindMap = Map.insert fname (ContFn f) (bindMap x) })
          return f
       emitRtsDeclarations =
         let go (fname, argTys, retTy) = extern fname argTys retTy >>= \f ->
               modify (\x->x{ bindMap = Map.insert fname (ContFn f) (bindMap x) })
               >> return f
         in mapM go rtsDecls
       genTopBinding (StgTopBind (StgBinding iden rhs)) = fnToLlvm iden rhs
       genTopBinding (StgTopData sumType) = genConstructor sumType
       genTopBinding (StgTopTypeDef nm ty) =
           modify (\x->x { typeMap = Map.insert nm ty (typeMap x) }) -- >> typedef nm Nothing
             >> (return $ ConstantOperand $ C.TokenNone) -- we must (?!) return an Operand
       emitModule :: CodeGenModuleConstraints m => m [Operand]
       emitModule = let order (StgTopBind _) = True
                        order _ = False
                        bindings = (\(StgTopBind b) -> b) <$> filter order topBindings
                    in registerBindings bindings -- handle forward references
                    *> emitRtsDeclarations
                    *> emitRtsConsts
                    *> mapM genTopBinding topBindings
       -- Override buildModuleT so we get more control over module parameters
       --myBuildModuleT :: ShortByteString -> ModuleBuilderT m a -> Module
       myBuildModuleT nm = fmap mkModule . execModuleBuilderT emptyModuleBuilder
         where mkModule ds = defaultModule { moduleName = nm
                                           , moduleDefinitions = ds
                                      --   , moduleTargetTriple = Just "x86_64-pc-linux-gnu"
                                           }
   in evalState (myBuildModuleT "MainModule" emitModule) stgToIRState

-- suspicious function - sometimes a vanilla stgType/type alias is expected
-- eg in an extern declaration we cannot really accept anything more complex
mkLlvmTy :: (CodeGenModuleConstraints m)
  => StgType -> m LLVM.AST.Type
mkLlvmTy = \case
  StgLlvmType t -> return t
  StgTypeAlias iden -> gets (aliasToType iden . typeMap) >>= \case
    Nothing -> error ("unknown type alias: " ++ show iden)
    Just ty -> mkLlvmTy ty
  any -> error "expected vanilla llvm type or type alias"

-- We have to make a function for every top level binding, so we define this immediately.
fnToLlvm :: (CodeGenModuleConstraints m)
  => StgId -> StgRhs -> m Operand
fnToLlvm iden (StgRhsClosure args argTypes retTy body) = mdo
    let getTy (StgLlvmType t) = return t
        getTy (StgTypeAlias iden) = maybe (return VoidType) getTy =<<
                                         gets (aliasToType iden . typeMap)
        getTy (StgTyperef t t2) = return t
        getTy (StgFnType types) = do
            llvmTypes <- mapM getTy types
            let (argTypes, [retType]) = splitAt (length llvmTypes - 1) llvmTypes
                isVarArg = False
                fnType = FunctionType retType argTypes isVarArg
                fnPtr = PointerType fnType (AddrSpace 0)
            return $ fnPtr
    params <- map (\ty->(ty, ParameterName "a")) <$> mapM getTy argTypes
    --argLlvmTys <- mapM getTy argTypes
    --params <- zipWithM (\ty name -> (ty, ParameterName name)) argLlvmTys args
    retTy' <- getTy retTy
    modify (\x->x { bindMap = Map.insert iden (ContFn f) (bindMap x) })
    f <- function iden params retTy' $ \llvmArgs ->
        -- fns bodies handled as: let args in fn
        let argBinds = zipWith StgBinding ((\(StgVarArg a) -> a) <$> args) -- ! litargs
                                          (StgRhsSsa <$> llvmArgs)
        in do -- sv State, codeGen a let, then clear the argument bindings
        svState <- get -- function args may squash bindings, so save and restore.
        ret =<< (stgToIR $ StgLet argBinds body)
        put svState
    return f
-- It must be a constant operand, this isn't checked
fnToLlvm iden (StgRhsSsa ssa)   = function iden [] (typeOf ssa) $ \[] -> ret ssa
fnToLlvm iden (StgRhsConst ssa) = global iden (LLVM.AST.Typed.typeOf ssa) ssa
fnToLlvm iden (StgExtern argTys retTy) = do
  argTys' <- mapM mkLlvmTy argTys
  retTy' <- mkLlvmTy retTy
  f <- extern iden argTys' retTy'
  modify (\x->x { bindMap = Map.insert iden (ContFn f) (bindMap x) })
  return f
fnToLlvm iden (StgExternVa argTys retTy) = do
  argTys' <- mapM mkLlvmTy argTys
  retTy' <- mkLlvmTy retTy
  f <- externVarArgs iden argTys' retTy'
  modify (\x->x { bindMap = Map.insert iden (ContFn f) (bindMap x) })
  return f

-- a let, but without the 'in expr'
-- (idea is to guarantee all top bindings are codegenned, even if unused)
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
stgToIR :: (MonadState (StgToIRState) m, MonadModuleBuilder m, MonadIRBuilder m, MonadFix m)
  => StgExpr -> m Operand

-- StgLet: register the bindings and let the 'expr' codegen them when it needs them.
stgToIR (StgLet bindList expr) = registerBindings bindList >> stgToIR expr

-- literals/functions with 0 arity
-- One might assume this is trivial, except we must emit constant arrays as globals
-- and call 0 arity functions (thunks), but not paps (>0 arity)
stgToIR (StgLit lit) =
  let handleArray :: CodeGenIRConstraints m => C.Constant -> m C.Constant
      handleArray arr@(C.Array _ _) = -- const arrays need to be emmited in global scope
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
             let zz = [(C.Int 32 0), (C.Int 32 0)]
             return $ C.GetElementPtr True (C.GlobalReference (ptr ty) nm) zz
      handleArray notArray = return notArray

      -- some literals are in fact 0 arity functions that must be resolved.
      callIfArity :: CodeGenIRConstraints m => Operand -> m Operand
      callIfArity s = case LLVM.AST.Typed.typeOf s of
          FunctionType _ [] _                 -> call s []
          PointerType (FunctionType _ [] _) _ -> call s []
          dont_call                          -> return s

      handle0Arity :: CodeGenIRConstraints m => StgId -> Symbol -> m Operand
      handle0Arity iden = \case
          ContLi s  -> return s --  never call literals
          ContFn v  -> callIfArity v
          ContRhs (StgRhsSsa val) -> return val -- don't make a function for literals
          ContRhs r -> fnToLlvm iden r >>= callIfArity -- rly
  in case lit of
     StgLitArg l ->  ConstantOperand <$> handleArray l
     StgSsaArg s ->  return s
     -- some varArgs are constructors or lazy closures that we must resolve (call) now.
     StgVarArg i ->  gets ((Map.!? i) . bindMap) >>= \case
                         Just v -> handle0Arity i v
                         Nothing -> error (show i ++ ": variable not found in bindMap")
     StgExprArg e -> stgToIR e

-- PrimOps = extract raw values and emit the primop's llvm instruction.
stgToIR (StgPrimOp (StgPrimBinOp op a b)) = do -- Second easiest case, primitives
  a' <- (stgToIR a)
  b' <- (stgToIR b)
  emitInstr (typeOf a') $ op a' b'

-- StgApp = arity >0 Function application (including Constructors)
-- Arities always exactly match
stgToIR (StgApp iden args) = do -- function application
  -- prepare arguments.
  ssaArgs <- mapM (stgToIR . StgLit) args
  let params = (\x->(x,[])) <$> ssaArgs -- the [] = parameter attributes
  -- Get the function. It might not yet be codegened, but should exist in our bindMap.
  gets ((Map.!? iden) . bindMap) >>= \case
     Just x -> case x of
      ContLi f -> call f params -- return f --error "StgToIR ($): panic: unexpected literal"
      ContFn f -> call f params
      ContRhs (StgRhsSsa val) -> call val params -- return val -- error "unexpected ssa: "
      ContRhs rhs@(StgRhsClosure _ _ _ _) -> fnToLlvm iden rhs >>= \f -> call f params
     Nothing -> error (show iden ++ ": function not found in bindMap")

-- | case: produce a switch on a literal value
stgToIR (StgCaseSwitch expr defaultBranch alts) = mdo
  let (vals, altCode) = unzip alts
  scrut <- stgToIR expr `named` "scrutinee"
  let genAlts expr = do -- collect (result,block) pairs to emit the phi instr
         b <- block
         r <- stgToIR expr
         br endBlock
         return (r, b)
  switchAlts <- return $ zip vals (snd $ unzip retBlockPairs)
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
  phi $ phiBlocks

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
           Just a -> case args of
               args -> stgToIR $ StgLet bindings a
           Nothing -> case alts of -- error "Product decon: expected expr"
               [_] -> stgToIR $ StgLet bindings e
               _ -> error "Product decon has multiple alts"

  -- Sum deconstruction: see genSumConstructor
  case alts of
      [] -> error "cannot deconstruct an empty type (?!)"
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
                      let --getSubTy (StructureType _ elems) = chk elems $ elems !! tagNumber
                          --chk elems = assert (length elems == length fieldNames)
                          --subTy = getSubTy ty
                          valPtrType = PointerType subTy (AddrSpace 0)
                      castStruct <- bitcast valPtr valPtrType `named` "cast"
                      val <- load castStruct 0
                      return $ (C.Int 32 $ fromIntegral tagNumber,
                               -- bitcast to data # tagNumber !
                                StgDataCase (StgLit (StgSsaArg val))
                                            (Just expr)
                                            [subProductType]
                                )
                  _ -> error "Panic: No subtype"
          taggedProducts <- zipWithM mkProduct [0..nAlts - 1] alts
          stgToIR (StgCaseSwitch (StgLit $ StgSsaArg tag) Nothing taggedProducts)

-- genConstructor
-- add to bindMap: llvm function that packs it's args into a struct/array/whatever
-- In general (barring astute optimization opportunities)
-- productTypes become a struct
-- sumTypes become a tag + void* pointer (for bitcasting purposes).
-- these clowns are both allowed to reference themselves, in which case they
-- become lazy linked lists on the heap, without special handling of this case
genConstructor :: CodeGenModuleConstraints m  => StgData -> m Operand
genConstructor = genSumConstructor

-- Sum types consist of a tag and embedded product types
-- Product types need to call the sum constructor to become 'embedded' in the sum type
-- embedded product types have the same type as the outer sum constructor.
-- TODO use this instead of `Maybe (Operand, Operand)`
data EmbedSum
 = EmbedSum Operand       -- Constructor Function
            Operand       -- 
            LLVM.AST.Type -- return type
                 

-- Generate the constructor function, then the deconstructor codegen-function.
-- The Maybe (Operand, Operand) is Just if this product is part of a sum type
genProductConstructor :: CodeGenModuleConstraints m 
  => StgProductType -> Maybe (Operand, Operand) -> m Operand
genProductConstructor algData@(StgProductType name fields) sumConstructor = do
  -- generate the constructor function
  let getType :: CodeGenModuleConstraints m => StgType -> m LLVM.AST.Type
      getType (StgLlvmType t) = return t
      getType (StgTyperef t t2) = return t
      getType (StgTypeAlias iden) = inMapOrOpaque iden =<< gets (aliasToType iden . typeMap)
      getType (StgAlgType stgData@(StgSumType iden alts)) = do
        conInfo <- gets (aliasToType iden . typeMap)
        case conInfo of
          Just (StgLlvmType ty) -> return ty
          Nothing -> do genConstructor stgData
                        >> getType (StgAlgType (StgSumType iden alts))
      inMapOrOpaque iden val = maybe (typedef iden Nothing) getType val
  types <- mapM getType fields
  let structType = StructureType packStruct types where packStruct = False
      nMembers = fromIntegral $ length types
      structName = name
      params = map (\x -> (x, ParameterName "A")) types
      conFnName = name
  let extractFnRetType (PointerType (FunctionType r _ _) _) = r
  structType <- typedef structName (Just structType)
  let retType = case sumConstructor of
          Nothing -> PointerType structType (AddrSpace 0)
          -- extract function ret type from the function pointer
          Just (conFn,tag) -> extractFnRetType $ LLVM.AST.Typed.typeOf conFn
      mainType = retType -- PointerType retType (AddrSpace 0)
      subType  = PointerType structType (AddrSpace 0)
      deCon = StgTyperef mainType subType
  -- Otherwise "Global variable initializer type does not match global variable type"
  -- We're apparently not allowed to overlap names of typedefs and global variables
  -- let nm = Name $ pack $ (BS.c2w '.') : ((\(Name s) -> unpack s) structName)
  -- structType <- typedef structName (Just $ (\(PointerType x _) -> x) retType)
  modify (\x->x { typeMap = Map.insert name deCon (typeMap x) })

  -- generate llvm constructor function (or global constant if it takes no params)
  -- if empty product type (within a sum type) -> make a global constant, not a function.
  let genCon [] = case sumConstructor of -- emit a global constant
          Nothing -> error "empty product type"
          Just (conFn, tag) -> do
             let voidPtr = C.IntToPtr (C.Int 32 0) (PointerType (IntegerType 8) (AddrSpace 0))
                 val = C.Struct (Just structName) False [C.Int 32 0, voidPtr]
                 sumType = (\(PointerType t _) -> t) retType
             g <- global conFnName sumType val
             modify (\x->x { bindMap = Map.insert name (ContLi g) (bindMap x) })
             return g

      genCon _ = -- generate constructor function
       let fnBody conArgs = do
             malloc <- gets ((\(Just (ContFn f)) -> f) . (Map.!? "malloc") . bindMap)
             let sz = ConstantOperand $ sizeof structType
             mem <- call malloc [(sz, [])]
             structPtr <- bitcast mem (PointerType structType (AddrSpace 0))
             let storeVal ssa idx = gep structPtr [constZero, ConstantOperand $ C.Int 32 idx]
                     >>= \ptr -> store ptr 0 ssa
             zipWithM storeVal conArgs [0..]
             case sumConstructor of
                 Nothing -> ret structPtr
                 Just (conFn, tag) -> do
                    voidPtr <- bitcast structPtr (PointerType (IntegerType 8) (AddrSpace 0))
                    ret =<< call conFn [(tag,[]), (voidPtr,[])]
       in function conFnName params retType fnBody >>= \conFn ->
          modify (\x->x { bindMap = Map.insert name (ContFn conFn) (bindMap x) })
          >> return conFn
  genCon params

-- Sum types become a struct containing a tag and void* pointer
-- the pointer can be bitcasted based on the tag in case expressions
-- constant tag switches are easily removed by llvm
-- in the pattern `Data Eg = Con (a0...an) | End` we can use nullptr instead of a tag.
-- This is important as it's the same pattern as the omnipresent [] type.
genSumConstructor :: CodeGenModuleConstraints m  => StgSumType -> m Operand

-- Only one alt = this sum type is actually a product type
genSumConstructor (StgSumType name [productType]) = genProductConstructor productType Nothing

genSumConstructor algData@(StgSumType name productTypes) = do
  -- Start by defining the list type as {i32, i8*} (tag and pointer)
  -- so it can handle recursive definitions
  let voidPtrType = (PointerType (IntegerType 8) (AddrSpace 0))
      tagType = IntegerType 32
      structName = name
      conFnName = name
      nMembers = 2
      structType  = StructureType packStruct [tagType, voidPtrType] where packStruct = False
  -- structType <- typedef structName (Just structType)
  let structPtrType = (PointerType structType (AddrSpace 0))
  modify (\x->x { typeMap = Map.insert name (StgLlvmType structPtrType) (typeMap x) })

  let params = [(tagType, ParameterName "tag"), (voidPtrType, ParameterName "unionPtr")]
  -- generate llvm constructor function
  -- altFn: gen BasicBlocks for the constructor function
  let altFn :: (CodeGenModuleConstraints m) => [Operand] -> IRBuilderT m ()
      --altFn [tag] = ret $ ConstantOperand $ C.Null AST.void -- eg. data Eg = Nothing
      altFn [tag, valVoidPtr] = do
          malloc <- gets ((\(Just (ContFn f)) -> f) . (Map.!? "malloc") . bindMap)
          let sz = ConstantOperand $ sizeof structType
          mem <- call malloc [(sz, [])]
          unionPtr <- bitcast mem (PointerType structType (AddrSpace 0))
          tagPtr <- gep unionPtr [constZero, constZero]
          store tagPtr 0 tag
          unionValPtr <- gep unionPtr [constZero, ConstantOperand $ C.Int 32 1]
          store unionValPtr 0 valVoidPtr
          ret unionPtr -- return this no matter what.

  conFn <- function conFnName params structPtrType $ altFn
  modify (\x->x { bindMap = Map.insert name (ContFn conFn) (bindMap x) })

  let getType (StgProductType deConId structTy) tag = do
        conInfo <- gets (aliasToType deConId . typeMap)
        case conInfo of
          Just ty -> error "Constructor already exists with that name"
          Nothing -> let product = StgProductType deConId structTy
                         tag' = ConstantOperand $ C.Int 32 $ fromInteger tag
                     in genProductConstructor product (Just (conFn, tag'))
  zipWithM_ getType productTypes [0..] -- gen ungenned product constructors
  return conFn
