module Repl.Loader where

import           System.Directory
import           System.FilePath
import           Control.Monad.IO.Class
import           Language.Haskell.Interpreter
import           Control.Monad.State.Lazy
import           Control.Lens

import           Repl.Types
import           Testing.TestExpExtractor
import           StaticAnalysis.CheckState

--todo: better path handling
loadModule :: FilePath -> Repl String
loadModule fn = do
  transModule <- liftIO $ transformFile fn
  let (dir, base) = splitFileName fn
      cdir        = dir </> ".drhaskell"
      cfn         = cdir </> base
  liftIO $ createDirectoryIfMissing False cdir
  liftIO $ writeFile cfn transModule
  liftInterpreter $ loadModules [cfn]
  liftInterpreter $ setTopLevelModules ["Main"]
  liftRepl $ modify $ Control.Lens.set filename fn
  liftIO $ runChecksL1 cfn
