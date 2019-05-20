-- Very cheap parser for testing purposes
{-# LANGUAGE TupleSections #-}
module Parser (parseProg)
where

-- Local imports
import Lexer -- (literal, ident)
import StgSyn

--
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative.Permutations
import Control.Monad.Combinators.Expr

import qualified Data.ByteString.Short as BS hiding (length) -- (pack, unpack)
import qualified Data.ByteString.Internal as BS (c2w) -- ghc won't autoconvert Char to Word8
import qualified LLVM.AST.Constant as C
import LLVM.AST -- (Operand(..))
import LLVM.AST.Type
import LLVM.AST.Typed
import LLVM.AST.Float ( SomeFloat(Double) )
import LLVM.AST.Instruction
import LLVM.AST.AddrSpace

-- for reading LLVM.AST types directly
-- obviously it would be preferable not to rely on the llvm-hs-pure read instances
import Text.Read (readMaybe)
import Control.Monad (when)
import Data.Void
import Data.Char (ord)

import Debug.Trace
import Text.Megaparsec.Debug

parseProg :: String -> String -> Either (ParseErrorBundle String Void) [StgTopBinding]
parseProg = parse (between sc eof stgProg)

stgProg :: Parser [StgTopBinding]
stgProg = indent topBinding `sepEndBy` many (lexeme $ oneOf ";\n" ) <?> "StgTopBinding"
  where indent = L.nonIndented scn . lexeme
        topBinding = StgTopData <$> stgData
                 <|> StgTopBind <$> stgBinding
                 <|> StgTopBind <$> stgExtern
                 <|> stgTypeAlias
        stgTypeAlias = StgTopTypeDef
                   <$  reserved "type"
                   <*> stgConstructor
                   <*  reservedOp "="
                   <*> stgType

stgExtern :: Parser StgBinding
stgExtern = lexeme (basicExtern <|> varArgExtern)
  where basicExtern = do
            reserved "extern"
            (name, argTys, retTy) <- lexeme stgTypeSignature
            some $ lexeme $ oneOf ";\n"
            return $ StgBinding name (StgExtern argTys retTy)
            <?> "extern declaration"
        varArgExtern = do
            reserved "externVarArg"
            (name, argTys, retTy) <- lexeme stgTypeSignature
            some $ lexeme $ oneOf ";\n"
            when (null argTys) (fail "expected at least one argument")
            return $ StgBinding name (StgExternVa argTys retTy)
            <?> "externVarArg declaration"
-- Top level bindings dont't need a 'let' nor a 'in expr'
stgBinding :: Parser StgBinding
stgBinding = do
  optional $ reserved "let"
  -- maybeSignature <- option Nothing (Just <$> try (lexemen stgTypeSignature))
  signature <- lexemen stgTypeSignature
  bindName <- lexeme stgId
  args <- many stgArgDef
  let (tyName, tyArgs, tyRet) = signature
  when (tyName /= bindName || length tyArgs /= length args) $
       fail "Type signature does not match binding" -- : "++tyName++" ~ "++bindName)
  reservedOp "="
  r <- lexeme $ stgRhs args (tyArgs, tyRet)
  return $ StgBinding bindName r
  <?> "Top level binding"

stgId, stgLowerId, stgConstructor :: Parser StgId -- StgId
stgId = stgLowerId <|> stgConstructor
stgLowerId     = Name . BS.pack . map BS.c2w <$> identifier
stgConstructor = Name . BS.pack . map BS.c2w <$> constructor

stgType :: Parser StgType
stgType = StgLlvmType <$> lexeme llvmType
      <|> StgTypeAlias <$> stgConstructor
      <|> StgFnType    <$> stgFunctionType
  where
  llvmType = 
      let mkPtr x = PointerType x (AddrSpace 0)
      in try readLlvmType
         <|> reserved "ptr" *> (mkPtr <$> llvmType)
         <|> mkPtr (IntegerType 8) <$ string "CharPtr"
         <|> mkPtr (IntegerType 8) <$ string "CStr"
         <?> "llvm Type"
     --- <|> (IntegerType 32) <$ string "Int"
     --- <|> (FloatingPointType (DoubleFP)) <$ string "Double"

  readLlvmType :: Parser LLVM.AST.Type.Type
  readLlvmType = maybe (fail "expected llvm type") return
                        =<< (readMaybe <$> manyTill L.charLiteral (oneOf ";\n"))
                        <?> "llvm Type"
  stgFunctionType = parens (stgType `sepBy1` reservedOp "->")

stgTypeSignature :: Parser (StgId, [StgType], StgType)
stgTypeSignature = do
  name <- lexeme stgId
  reservedOp "::"
  types <- stgType `sepBy1` reservedOp "->"
  let (argTypes, [retTy]) = splitAt (length types - 1) types
  return (name, argTypes, retTy) <* sc
  <?> "Type signature"

stgData :: Parser StgData
stgData = stgSumType
  where 
        stgSumType = 
          StgSumType
          <$  reserved "data"
          <*> stgConstructor
          <*  reservedOp "="
          <*> stgProductDef `sepBy` (reservedOp "|")
          -- <*  oneOf ";\n"
          <?> "data"
         where
         stgProductDef :: Parser StgProductType
         stgProductDef = StgProductType
                     <$> stgConstructor
                     <*> many (lexeme stgType) -- `sepBy` space

stgArgDef :: Parser StgArg
stgArgDef = StgVarArg <$> (parens stgId <|> stgId)

stgRhs :: [StgArg] -> ([StgType], StgType) -> Parser StgRhs
stgRhs args (argTypes, retTy) =
      StgRhsClosure args argTypes retTy <$> try stgExpr
  <|> StgRhsSsa . ConstantOperand <$> stgConst
  <?> "StgRhs"

stgExpr :: Parser StgExpr
stgExpr = stgLet
      <|> stgCase
      <|> stgDecon
      <|> stgPrimitive
      <?> "StgExpr"

stgApp :: Parser StgExpr
stgApp = StgApp
     <$> (stgId <|> stgConstructor)
     <*> many (try (notPrimitive *> lexeme stgCallArg))
     <?> "StgLet"
    where
    -- (f - 3) could parse as (f (-3)), we need to check there isn't an operator after the arg.
    notPrimitive = notFollowedBy (choice . map reservedOp $ reservedOps)
    stgCallArg = (StgExprArg  <$> parens stgExpr)
             <|> (StgConstArg <$> stgConst)
             <|> (StgVarArg   <$> stgId)
             <?> "StgCallArg"
    -- stgForeign

stgLet :: Parser StgExpr
stgLet = StgLet
     <$  reserved "let"
     <*> try stgBinding `sepEndBy1` some (lexeme $ oneOf ";\n")
     <*  lexemen (reserved "in")
     <*> lexeme stgExpr 
     <?> "StgLet"
stgCase :: Parser StgExpr
stgCase = do
  let prevIndent = mkPos 1 --L.indentLevel
  reserved "case"
  scrut <- stgPrimitive
  reserved "of"
  (alts, defaultAlt) <- bracesCase <|> indentCase prevIndent
  return $ StgCaseSwitch scrut defaultAlt alts
  <?> "StgCase"
      where
      bracesCase = between (symboln "{") (symboln "}") $ do
            alts <- stgCaseAlt `sepEndBy` (symboln ";")
            defaultAlt <- optional $ lexemen stgCaseDefault <* optional (symboln ";")
            return (alts, defaultAlt)
      indentCase prevIndent = do
            (eol *> L.indentGuard sc GT prevIndent)
            newIndent <- L.indentLevel
            x <- lexeme stgCaseAlt
            -- we can't use 'some' because we must save the indentation first
            xs <- many $ try (eol *> L.indentGuard sc EQ newIndent *> stgCaseAlt)
            ((x:xs), ) <$> optional (eol *> L.indentGuard sc EQ newIndent *> stgCaseDefault)
      stgCaseDefault = reserved "_"
                    *> reservedOp "->"
                    *> lexeme stgExpr
                    <?> "StgCaseDefault"
      stgCaseAlt = do
          notFollowedBy stgCaseDefault
          val <- stgPrimitive
          reservedOp "->"
          alt <- stgExpr
          let unbox (StgLit (StgConstArg c)) = c
              val' = unbox val
          return (val', alt)

stgDecon :: Parser StgExpr
stgDecon = do
  let prevIndent = mkPos 1
  reserved "decon"
  scrut <- stgPrimitive
  reserved "of"
  let bracesCase = (between (symboln "{") (symboln "}")) $ stgDeconAlt `sepBy` (symboln ";")
  alts <- bracesCase <|> indentCase prevIndent
  return $ StgDataCase scrut Nothing alts
      where
      indentCase prevIndent = do
          let indentAlt = do
                  deCon <- stgId
                  args <- many stgId
                  reservedOp "->"
                  e <- lexeme stgExpr
                  return (deCon, args, e)
          (eol *> L.indentGuard sc GT prevIndent)
          newIndent <- L.indentLevel
          x <- indentAlt
          xs <- many $ try (eol *> L.indentGuard sc EQ newIndent *> indentAlt)
          return (x : xs)

      stgDeconAlt :: Parser (StgId, [StgId], StgExpr)
      stgDeconAlt = do
          deCon <- stgId
          args <- many stgId
          reservedOp "->"
          e <- lexemen stgExpr
          return (deCon, args, e)

-- a 'primitive' here is anything except 'let' and 'case' expressions
stgPrimitive :: Parser StgExpr
stgPrimitive = makeExprParser term table 
  where
  -- A little messy since Instruction constructors have different kinds (some take flags)
  -- The Stg wants a partial constructor (f :: Operand -> Operand -> LLVM.Instruction)
  table = [[ binary "*." (mkInstr (\x y -> FMul noFastMathFlags x y []))
           , binary "/." (mkInstr (\x y -> FDiv noFastMathFlags x y []))]
          ,[ binary "+." (mkInstr (\x y -> FAdd noFastMathFlags x y []))
           , binary "-." (mkInstr (\x y -> FSub noFastMathFlags x y []))]
          ,
           [ binary "*" (mkInstr (\x y -> Mul False False x y []))
           , binary "/" (mkInstr (\x y -> SDiv False x y []))]
          ,[ binary "+" (mkInstr (\x y -> Add False False x y []))
           , binary "-" (mkInstr (\x y -> Sub False False x y []))]
          ]
      where
      --prefix s f = Prefix (reservedOp s >> return $ StgPrimOp f)
      binary s f = InfixL (reservedOp s >> return f)
      mkInstr f x y = StgPrimOp $ StgPrimBinOp f x y
  term :: Parser StgExpr
  term = let removeNullStgApps (StgApp a []) = StgLit $ StgVarArg a
             removeNullStgApps e = e
         in parens stgExpr
        <|> removeNullStgApps <$> stgApp -- if an app on [], ignore
        <|> StgLit . StgConstArg <$> stgConst
        <|> StgLit . StgVarArg <$> stgId
        <?> "StgPrimitive"
stgConst :: Parser StgConst
stgConst = let mkFloat  = C.Float . LLVM.AST.Float.Double
               mkInt    = C.Int 32
               mkChar   = C.Int 8 . fromIntegral . ord
               mkString s = C.Array (IntegerType 8) (map mkChar s)
           in 
              mkString <$> stringLiteral
          <|> mkChar   <$> charLiteral
          <|> mkInt    <$> try Lexer.int
          <|> mkFloat  <$> Lexer.double
          <?> "StgConst"
