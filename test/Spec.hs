--import Test.Tasty
import Test.Tasty.HUnit
import Test.Framework (defaultMain, testGroup)

import System.Directory
import System.Path.NameManip
import Test.Framework.Providers.Program
import System.Process
import Data.List (sort)

import Debug.Trace
main =
  let exampleDir = "examples/"
      stgCompiler = "./lfvm"
      -- mkTestCase :: (FilePath, FilePath) -> Test
      mkTestCase (stgFile, out) = testProgramOutput stgFile stgCompiler [stgFile, "--jit"] (Just (==out)) Nothing
  in do
  system "make" -- make sure lfvm is freshly built
  examples <- getOutStgPairs <$> listDirectory exampleDir :: IO [(FilePath, FilePath)]
  contents <- withCurrentDirectory exampleDir $
              mapM (\(out, stgFile) -> sequence (exampleDir ++ stgFile, readFile out)) examples
  defaultMain $ [testGroup "diff <(./lfvm --jit *.stg) *.out" $ mkTestCase <$> contents]

-- Get all [(.out, .stg)] filePairs from a list of files
getOutStgPairs :: [FilePath] -> [(FilePath, FilePath)]
getOutStgPairs = pairWithOuts [] . sort
 where
  pairWithOuts :: [(FilePath, FilePath)] -> [FilePath] -> [(FilePath, FilePath)]
  pairWithOuts acc (out : stg : l) =
    let (outnm, outSfx) = split_filename out
        (stgnm, stgSfx) = split_filename stg
    in case outnm == stgnm && outSfx == "out" && stgSfx == "stg" of
      True -> pairWithOuts ((out, stg) : acc) l
      False-> pairWithOuts acc (stg : l)
  pairWithOuts acc _ = acc
