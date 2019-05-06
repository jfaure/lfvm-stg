module CmdLine (CmdLine(..), parseCmdLine, cmdLineDefaults)
where

import Options.Applicative
import Data.Semigroup ((<>))

data CmdLine = CmdLine
  { emitStg   :: Bool
  , emitLlvm  :: Bool
  , jit       :: Bool
  , optlevel  :: Word
  , files     :: [String]
  }

cmdLineDefaults = CmdLine
 { emitStg  = False
 , emitLlvm = True
 , jit      = False
 , optlevel = 0
 , files    = []
 }

cmdLineDecls :: Parser CmdLine
cmdLineDecls = CmdLine
  <$> switch (long "emit-stg"
          <> short 's'
          <> help "Output program ast")
  <*> switch (long "emit-llvm"
          <> short 'l'
          <> help "Output llvm disassembly")
  <*> switch (long "jit"
          <> short 'j'
          <> help "execute program in jit")
  <*> option auto (short 'O'
               <> help "optimization level = 0|1|2|3"
               <> value 0)
  <*> many (argument str (metavar "FILE"))

cmdLineInfo = info (cmdLineDecls <**> helper) description
  where description = fullDesc
                   <> progDesc "lfvm-stg compiler"
                   <> header "lfvm"

parseCmdLine :: IO CmdLine
parseCmdLine = execParser cmdLineInfo
