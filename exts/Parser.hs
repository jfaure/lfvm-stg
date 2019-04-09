{-# LANGUAGE OverloadedStrings#-}
-- Very cheap parser for testing purposes
-- Every number is parsed as a Double, no questions asked,
-- although of course any soup of valid llvm type combinations would be handled the same way
  module Parser where

-- Local imports
import Lexer -- (literal, ident)
import StgSyn

--
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Control.Applicative (liftA2)
import Data.ByteString.Char8 (pack)
import qualified LLVM.AST.Constant as C
import LLVM.AST -- (Operand(..))
import LLVM.AST.Type
import LLVM.AST.Float ( SomeFloat(Double) )
import LLVM.AST.Instruction

import Debug.Trace

parseProg :: String -> Either ParseError [StgBinding]
parseProg s = parse (between (Tok.whiteSpace lexer) eof stgProg) "<stdin>" s

stgProg :: Parser [StgBinding]
stgProg = stgBinding `sepBy` (oneOf "\n;") <?> "StgProg"

-- Top level bindings dont't need a 'let' nor a 'in expr'
stgBinding :: Parser StgBinding
stgBinding = do
  optional $ reserved "let"
  b <- identifier
  args <- many stgArgDef
  reservedOp "="
  r <- stgRhs b args
  return $ StgBinding (pack b) r

stgId :: Parser StgId -- StgId
stgId = pack <$> identifier

stgArgDef :: Parser StgArg
stgArgDef = StgVarArg <$> (parens stgId <|> stgId)

stgRhs :: String -> [StgArg] -> Parser StgRhs
stgRhs iden args =
  let ty = LLVM.AST.FloatingPointType DoubleFP
      argTypes = ty <$ args
  in  (StgRhsSsa . ConstantOperand) <$> stgConst
  <|> (StgRhsClosure Updateable args (argTypes) ty) <$> (stgExpr)
  <?> "StgRhs"

stgExpr :: Parser StgExpr
stgExpr = stgLet
      <|> stgCase
      <|> stgPrimitive
      <?> "StgExpr"

stgApp :: Parser StgExpr
stgApp = StgApp
     <$> stgId
     <*> (many (notPrimitive *> stgCallArg))
     <?> "StgLet"
    where
    -- (f - 3) could parse as (f (-3)), we need to check there isn't an operator after the arg.
    notPrimitive = notFollowedBy (choice . map reservedOp $ reservedOps)
    stgCallArg = (StgExprArg <$> parens stgExpr)
             <|> (StgLitArg  <$> stgConst)
             <|> (StgVarArg  <$> stgId)
             <?> "StgCallArg"
    -- stgForeign

stgLet :: Parser StgExpr
stgLet = StgLet
    <$  reserved "let"
    <*> stgBinding `sepBy1` (oneOf "\n;")
    <*  reserved "in"
    <*> stgExpr
    <?> "StgLet"
stgCase :: Parser StgExpr
stgCase = do
    reserved "case"
    scrut <- stgPrimitive
    reserved "of"
    (alts, defaultAlt) <- Tok.braces lexer $ do
        alts <- (notFollowedBy stgCaseDefault *> stgCasealt) `sepEndBy` (Tok.semi lexer)
        defaultAlt <- stgCaseDefault
        return (alts, defaultAlt)
    return $ StgCaseSwitch scrut defaultAlt alts
    <?> "StgCase"
        where
        stgCaseDefault = reserved "_"
                      *> reservedOp "->"
                      *> stgExpr
        stgCasealt = do
            val <- stgPrimitive
            reservedOp "->"
            alt <- stgExpr
            let unbox (StgLit (StgLitArg c)) = c
                val' = unbox val
            return (val', alt)

-- a 'primitive' here is anything except 'let' and 'case' expressions
stgPrimitive :: Parser StgExpr
stgPrimitive = Ex.buildExpressionParser table term
  where
  -- A little messy since Instruction constructors have different kinds (some take flags)
  -- The Stg wants a partial constructor (f :: Operand -> Operand -> LLVM.Instruction)
  table = [[ binary "*" (mkInstr (\x y -> FMul noFastMathFlags x y []))
           , binary "/" (mkInstr (\x y -> FDiv noFastMathFlags x y []))]
          ,[ binary "+" (mkInstr (\x y -> FAdd noFastMathFlags x y []))
           , binary "-" (mkInstr (\x y -> FSub noFastMathFlags x y []))]
          ]
      where
      --prefix s f = Ex.Prefix (reservedOp s >> return $ StgPrimOp f)
      binary s f = Ex.Infix (reservedOp s >> return f) Ex.AssocLeft
      mkInstr f x y = StgPrimOp $ StgPrimBinOp f x y
  term :: Parser StgExpr
  term = parens stgExpr
     <|> stgApp
     <|> (StgLit . StgLitArg) <$> stgConst
     <|> (StgLit . StgVarArg) <$> stgId
     <?> "StgPrimitive"
stgConst :: Parser StgConst
stgConst = (C.Float . LLVM.AST.Float.Double)
       <$> (fromInteger <$> try Lexer.int <|> Lexer.double)
       <?> "StgConst"
