{-# language OverloadedStrings, FlexibleContexts #-}
module Jit (runJIT) where

import Data.Int
import Data.Word
import Foreign.Ptr ( FunPtr, castFunPtr )

import Control.Monad.Except

import LLVM.Target
import LLVM.Context
import LLVM.CodeModel
import LLVM.Module as Mod
import qualified LLVM.AST as AST

import LLVM.PassManager
import LLVM.Transforms
import LLVM.Analysis

import qualified LLVM.ExecutionEngine as EE

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> IO Double

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: Word -> PassSetSpec
passes opt = defaultCuratedPassSetSpec { 
    optLevel = if opt > 0 then Just opt else Nothing
}

runJIT :: Word -> Bool -> AST.Module -> IO AST.Module
runJIT opt execute mod =
  withContext $ \context ->
    jit context $ \executionEngine ->
      withModuleFromAST context mod $ \m -> do
-- rip default pass managers until this issue is addressed
-- https://github.com/Wilfred/bfc/issues/27
     -- withPassManager (passes opt) $ \pm -> do
     --   runPassManager pm m -- optimization
          verify m -- sanity
          optmod <- moduleAST m
          when execute $ exec executionEngine m
          return optmod
  where
    exec engine m = EE.withModuleInEngine engine m $ \ee -> do
      mainfn <- EE.getFunction ee (AST.Name "main")
      case mainfn of
        Just fn -> do
          res <- run fn
        --  putStrLn $ "exited with: " ++ (show $ round res)
          return ()
        Nothing -> return ()
