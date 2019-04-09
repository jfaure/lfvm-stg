{-# LANGUAGE LambdaCase #-}
-- Very cheap frontend for testing purposes -
-- the input program is assumed to be coherent and
-- we attempt to codegen anything that parses !
module Main where

import Parser
import StgSyn
import StgToLLVM (stgToIRTop)

import System.Environment (getArgs)
import Control.Monad.Trans (lift)
import System.Console.Haskeline
import LLVM.AST (Module)

import LLVM.Pretty (ppllvm)
import qualified Data.Text.Lazy.IO as TIO

main :: IO ()
main = getArgs >>= \case
  []  -> repl
  av  -> mapM_ ((go =<<) . readFile) av

goFile = (go =<<) . readFile

go :: String -> IO ()
go progStr = case parseProg progStr of
    Right stg -> TIO.putStrLn $ ppllvm $ stgToIRTop stg
    Left err  -> print err

repl :: IO ()
repl = runInputT defaultSettings loop
 where
  loop = getInputLine "stg-jit> " >>= \case
    Nothing -> return ()
    Just l  -> lift (go l) >> loop
