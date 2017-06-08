module Repl.Loader where

import           System.Directory
import           System.FilePath
import           Control.Monad.IO.Class
import           Language.Haskell.Interpreter
import           Control.Monad.State.Lazy
import           Control.Lens
import           Control.Monad.Catch

import           Repl.Types
import           Testing.TestExpExtractor
import           StaticAnalysis.CheckState
import           StaticAnalysis.Messages.StaticErrors
import           Language.Haskell.Exts

--todo: better path handling
loadModule :: FilePath -> Repl [Error SrcSpanInfo]
loadModule fn = do
  transModule <- liftIO $ transformFile fn
  let (dir, base) = splitFileName fn
      cdir        = dir </> ".drhaskell"
      cfn         = cdir </> base
  liftIO $ createDirectoryIfMissing False cdir
  liftIO $ writeFile cfn transModule
  -- errors <- liftIO $ runChecksL1 cfn
  -- throwM errors
  errors <- liftIO $ runChecksL1 fn
  if null errors
  then do
    liftInterpreter $ loadModules [cfn]
    liftInterpreter $ setTopLevelModules ["Main"]
    liftRepl $ modify $ Control.Lens.set filename fn
    return []
  else return errors
