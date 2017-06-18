module Repl.Loader (module Repl.Loader) where

import           Control.Lens                         hiding (Level)
import           Control.Monad.Catch                  as MC
import           Control.Monad.IO.Class
import           Control.Monad.State.Lazy
import           Language.Haskell.Interpreter
import           System.Directory
import           System.FilePath

import           Language.Haskell.Exts
import           Util.ModifyAst
import           Repl.Types
import           StaticAnalysis.CheckState
import           StaticAnalysis.Messages.StaticErrors
import qualified Testing.TestExpExtractor as Tee

--todo: better path handling
loadModule :: FilePath -> Repl [Error SrcSpanInfo]
loadModule fn = do
  nonstrict <- use nonStrict
  pr <- liftIO $ parseModified fn
  case pr of
    ParseFailed l e -> return [SyntaxError (infoSpan (mkSrcSpan l l) []) e]
    ParseOk modLoad -> do
      let (dir, base) = splitFileName fn
          cdir        = dir </> ".drhaskell"
          cfn         = cdir </> base
      liftIO $ createDirectoryIfMissing False cdir

      Just level <- foldr (liftM2 mplus) (return $ Just Level1) [use forceLevel, return $ determineLevel modLoad]
      (transModule, transErrors) <- transformModule modLoad
      liftIO $ writeFile cfn $ printModified transModule

      checkErrors <- liftIO $ runCheckLevel level fn
      let errors = checkErrors ++ transErrors
      when (null errors || nonstrict) $ do
        liftInterpreter $ loadModules [cfn]
        liftInterpreter $ setTopLevelModules ["Main"]
        liftRepl $ modify $ Control.Lens.set filename fn
        rt <- use runTests
        when rt runAllTests
      return errors

determineLevel :: ModifiedModule -> Maybe Level
determineLevel = foldr (mplus . extractLevel) Nothing . modifiedComments
  where
    extractLevel :: Comment -> Maybe Level
    extractLevel (Comment _ _ "# DRHASKELL LEVEL1 #")    = Just Level1
    extractLevel (Comment _ _ "# DRHASKELL LEVEL2 #")    = Just Level2
    extractLevel (Comment _ _ "# DRHASKELL LEVEL3 #")    = Just LevelFull
    extractLevel (Comment _ _ "# DRHASKELL LEVELFULL #") = Just LevelFull
    extractLevel (Comment _ _ "# DRHASKELL FULL #")      = Just LevelFull
    extractLevel _                                       = Nothing

runAllTests :: Repl ()
runAllTests = MC.handleAll (\_ -> return ()) $
  liftInterpreter (interpret "runAllTests" (as :: IO ())) >>= liftIO

transformModule :: ModifiedModule -> Repl (ModifiedModule, [Error SrcSpanInfo])
transformModule = liftIO . Tee.transformModule