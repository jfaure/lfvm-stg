module Lexer where

import StgSyn

-- Lexer
--import Text.Parsec.Char (satisfy)
import Text.ParserCombinators.Parsec.Char (satisfy)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Combinator (option, lookAhead)
import qualified Text.Parsec.Token as Tok

import Control.Applicative (liftA2)
import Data.Char (isLower, isUpper)

-- map xs =
--     case xs of
--        Nil -> Nil
--        Cons y ys -> let fy = f y
--                         mfy = map f ys
--                       in Cons fy mfy

reservedOps = ["*","/","+","-", "=", "->"]
reservedNames = ["let", "case", "of", "_"]

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style = emptyDef {
               Tok.commentLine = "--"
--           , Tok.identStart = letter <|> char '_'
--           , Tok.identLetter = letter <|> char '_' <|> number
             , Tok.reservedOpNames = reservedOps
             , Tok.reservedNames = reservedNames
             }

int :: Parser Integer
int = Tok.integer lexer
double :: Parser Double
double = Tok.float lexer
literal_string :: Parser String
literal_string = (Tok.stringLiteral lexer)

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

-- must start with lowercase, or it's a constructor
identifier :: Parser String
identifier = lookAhead (satisfy isLower) >> Tok.identifier lexer

-- constructors always start with uppercase
constructor :: Parser String
constructor = lookAhead (satisfy isUpper) >> Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
