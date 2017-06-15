module Repl.Loader where

import           Control.Lens                         hiding (Level)
import           Control.Monad.Catch                  as MC
import           Control.Monad.IO.Class
import           Control.Monad.State.Lazy
import           Language.Haskell.Interpreter
import           System.Directory
import           System.FilePath

import           Language.Haskell.Exts
import           Repl.Types
import           StaticAnalysis.CheckState
import           StaticAnalysis.Messages.StaticErrors
import           Testing.TestExpExtractor

--todo: better path handling
loadModule :: FilePath -> Repl [Error SrcSpanInfo]
loadModule fn = do
  Just level <- foldr (liftM2 mplus) (return $ Just Level1) [use forceLevel, determineLevel fn]
  nonstrict <- use nonStrict
  transModule <- liftIO $ transformFile fn
  let (dir, base) = splitFileName fn
      cdir        = dir </> ".drhaskell"
      cfn         = cdir </> base
  liftIO $ createDirectoryIfMissing False cdir
  liftIO $ writeFile cfn transModule
  errors <- liftIO $ runCheckLevel level fn
  when (null errors || nonstrict) $ do
    liftInterpreter $ loadModules [cfn]
    liftInterpreter $ setTopLevelModules ["Main"]
    liftRepl $ modify $ Control.Lens.set filename fn
    rt <- use runTests
    when rt runAllTests
  return errors

determineLevel :: FilePath -> Repl (Maybe Level)
determineLevel fn = do
  ast <- liftIO $ parseFileWithComments defaultParseMode fn
  return $ case ast of
    ParseFailed _ _      -> Nothing
    ParseOk (_,comments) -> foldr (mplus . extractLevel) Nothing comments
  where
    extractLevel :: Comment -> Maybe Level
    extractLevel (Comment _ _ "# DRHASKELL LEVEL1 #")    = Just Level1
    extractLevel (Comment _ _ "# DRHASKELL LEVEL2 #")    = Just Level2
    extractLevel (Comment _ _ "# DRHASKELL LEVEL3 #")    = Just LevelFull
    extractLevel (Comment _ _ "# DRHASKELL LEVELFULL #") = Just LevelFull
    extractLevel (Comment _ _ "# DRHASKELL FULL #")      = Just LevelFull
    extractLevel _                                       = Nothing

runAllTests :: Repl ()
runAllTests = MC.handleAll (\_ -> return ()) $ do
  liftInterpreter (interpret "runAllTests" (as :: IO ())) >>= liftIO
