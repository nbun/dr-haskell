module Repl.CmdOptions (
    handleCmdArgs
) where

import Control.Lens              as L
import Repl.Types
import StaticAnalysis.CheckState
import System.Console.GetOpt
import System.Environment

handleCmdArgs :: IO ReplState
handleCmdArgs = do
  argv <- getArgs
  case getOpt (ReturnInOrder readFilename) options argv of
    (o,n,[]  ) -> return $ foldl (flip id) initialReplState o
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo "" options))


options :: [OptDescr (ReplState -> ReplState)]
options = [
  Option ['l'] ["level"]
         (ReqArg readLevel "LEVEL")
         "Ignore level specified in file and use this instead",
  Option ['t'] ["no-test"]
         (NoArg $ runTests .~ False)
         "Do not automatically run tests",
  Option ['s'] ["no-strict"]
         (NoArg $ nonStrict .~ True)
         "Try to load module even if problems were found",
  Option ['p'] ["no-custom-prelude"]
         (NoArg $ customPrelude .~ False)
         "Use a standard Haskell Prelude instead of DrHaskell's",
  Option ['g'] ["ghc-options"]
         (ReqArg readGHCOpts "OPTS")
         "Additional GHC options like '-package-db='",
  Option ['n'] ["no-update-check"]
         (NoArg $ noUpdateCheck .~ True)
         "Disable update check at startup"]

readFilename :: String -> ReplState -> ReplState
readFilename fn state = case state ^. filename of
                             "" -> L.set filename fn state
                             _  -> error "Cannot load multiple files."

readLevel :: String -> ReplState -> ReplState
readLevel "1"    = forceLevel .~ Just Level1
readLevel "2"    = forceLevel .~ Just Level2
readLevel "3"    = forceLevel .~ Just Level3
readLevel "4"    = forceLevel .~ Just LevelFull
readLevel "full" = forceLevel .~ Just LevelFull
readLevel "FULL" = forceLevel .~ Just LevelFull
readLevel _      = error "invalid level" --TODO: improve this...

readGHCOpts :: String -> ReplState -> ReplState
readGHCOpts args = ghcOptions .~ words args
