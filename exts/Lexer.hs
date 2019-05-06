module Lexer where

import StgSyn

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Control.Monad (void)
import Data.Void
import Data.List (isInfixOf)

type Parser = Parsec Void String

-- Space consumers: scn eats newlines, sc does not.
lineComment = L.skipLineComment "--"
blockComment = L.skipBlockComment "{-" "-}"
scn :: Parser ()
scn = L.space space1 lineComment blockComment
sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) lineComment empty
  where f x = x == ' ' || x == '\t'

lexeme, lexemen :: Parser a -> Parser a
lexeme = L.lexeme sc
lexemen = L.lexeme scn
symbol, symboln :: String -> Parser String
symbol = L.symbol sc
symboln = L.symbol scn

terminator :: Parser ()
terminator = eof <|> ((some $ lexemen $ oneOf "\n;") >> return ())
reservedOps = ["*","/","+","-", "=", "->", "|", "->", "::"]
reservedNames = ["let", "in", "case", "decon", "of", "_", "data", "ptr",
                 "type", "extern", "externVarArg"]
reservedName w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)
reservedOp w = lexeme $ try $ (notFollowedBy (opLetter w) *> string w)
  where opLetter :: String -> Parser ()
        opLetter w = choice (string <$> (longerOps w)) >> return ()
        longerOps w = filter (\x -> isInfixOf w x && x /= w) reservedOps
reserved = reservedName

iden :: Parser String
iden = (lexeme . try) (p >>= check)
  where
  p = (:) <$> letterChar <*> many alphaNumChar
  check x = if x `elem` reservedNames
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x

-- constructors always start with uppercase, identifiers with lowercase
identifier, constructor :: Parser String
identifier  = lookAhead lowerChar >> iden
constructor = lookAhead upperChar >> iden
int :: Parser Integer
int = lexeme L.decimal
double :: Parser Double
double = lexeme L.float
-- L.charLiteral handles escaped chars automatically (eg. \n)
charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral
stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

parens, braces :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
