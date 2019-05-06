{-# LANGUAGE LambdaCase, MultiWayIf #-}
-- Very cheap frontend for testing purposes -
-- the input program is assumed to be coherent and
-- we attempt to codegen anything that parses !
module Main where

import CmdLine (CmdLine(..), cmdLineDefaults, parseCmdLine)
import Parser (parseProg)
import Text.Megaparsec (errorBundlePretty)
-- import StgSyn
import StgToLLVM (stgToIRTop)

import LLVM.AST (Module)
import LLVM.Pretty (ppllvm)

import Jit (runJIT)

import Control.Monad.Trans (lift)
import System.Console.Haskeline

import qualified Data.Text.Lazy.IO as TIO

main :: IO ()
main = parseCmdLine >>= \cmdLine ->
  case files cmdLine of
      [] -> repl cmdLine
      av -> mapM_ ((dispatch cmdLine =<<) . readFile) av

dispatch :: CmdLine -> String -> IO ()
dispatch c progStr = case parseProg "<stdin>" progStr of
  Left err  -> putStrLn $ errorBundlePretty err
  Right stg ->
   let llvmModule = stgToIRTop stg
   in if
    | emitStg c  -> print stg
    | emitLlvm c -> TIO.putStrLn $ ppllvm    llvmModule
    | jit c      -> runJIT (optlevel c) True llvmModule >> return ()
    | True       -> TIO.putStrLn $ ppllvm    llvmModule

repl :: CmdLine -> IO ()
repl cmdLine = runInputT defaultSettings loop
 where loop = getInputLine "stg-jit> " >>= \case
         Nothing -> return ()
         Just l  -> lift (dispatch cmdLine l) >> loop
