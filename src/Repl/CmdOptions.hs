module Repl.CmdOptions where

import           Control.Lens              as L
import           Repl.Types
import           StaticAnalysis.CheckState
import           System.Console.GetOpt
import           System.Environment

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
         "Try to load module even if problems were found"]

readFilename :: String -> ReplState -> ReplState
readFilename = L.set filename

readLevel :: String -> ReplState -> ReplState
readLevel "1"    = forceLevel .~ Just Level1
readLevel "2"    = forceLevel .~ Just Level2
readLevel "3"    = forceLevel .~ Just Level3
readLevel "full" = forceLevel .~ Just LevelFull
readLevel _      = error "invalid level" --TODO: improve this...

