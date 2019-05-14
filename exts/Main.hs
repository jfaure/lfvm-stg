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
import qualified LLVM.Module as M
import LLVM.Context
import qualified Data.ByteString.Char8 as B
import LLVM.PassManager
import LLVM.Transforms
import LLVM.Analysis

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
       withCppModule :: LLVM.AST.Module -> (M.Module -> IO a) -> IO a
       withCppModule astMod f = withContext $ \c ->
                                    M.withModuleFromAST c astMod (\m -> verify m >> f m)
       optimize :: M.Module -> (M.Module -> IO a) -> IO a
       optimize m f = withPassManager defaultPassSetSpec $ \pm -> do
                        runPassManager pm m
                        f m
       withCppOptMod astMod f = withCppModule astMod f --(`optimize` f)
   in if
   -- options on LLVM.AST.Module
    | emitStg c  -> print stg
    | emitLlvm c -> TIO.putStrLn $ ppllvm    llvmModule
    | jit c      -> runJIT (optlevel c) True llvmModule >> return ()
   -- need an M.Module
    | otherwise -> withCppOptMod llvmModule ((B.putStrLn =<<) . M.moduleLLVMAssembly)

repl :: CmdLine -> IO ()
repl cmdLine = runInputT defaultSettings loop
 where loop = getInputLine "stg-jit> " >>= \case
         Nothing -> return ()
         Just l  -> lift (dispatch cmdLine l) >> loop
