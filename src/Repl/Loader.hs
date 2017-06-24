{-# LANGUAGE FlexibleContexts #-}
module Repl.Loader (
  LoadMessage(..),
  printLoadMessage,
  determineLevel,
  transformModule,
  loadModule,
) where

import           Control.Lens                         hiding (Level)
import           Control.Monad.Catch                  as MC
import           Control.Monad.IO.Class
import           Control.Monad.State.Lazy             as MS
import           Language.Haskell.Interpreter
import           StaticAnalysis.StaticChecks.Select
import           System.Directory
import           System.FilePath

import           Language.Haskell.Exts
import           Repl.Types
import           StaticAnalysis.CheckState
import           StaticAnalysis.Messages.Prettify
import           StaticAnalysis.Messages.StaticErrors
import qualified Testing.TestExpExtractor             as Tee
import           Util.ModifyAst

data LoadMessage = CheckError (Error SrcSpanInfo)
                 | DirectMessage String

printLoadMessage :: LoadMessage -> String
printLoadMessage (CheckError e)    = prettyError e
printLoadMessage (DirectMessage m) = m

--todo: better path handling

loadModule :: FilePath -> Repl [LoadMessage]
loadModule fn = MC.handleAll handler $ loadModule' fn
  where
    -- handles IO errors thrown by parseModified
    handler e = return [DirectMessage (displayException e)]
    loadModule' fn' = do
      nonstrict <- use nonStrict
      pr <- liftIO $ parseModified fn'
      case pr of
        ParseFailed l e ->
          return [CheckError $ SyntaxError (infoSpan (mkSrcSpan l l) []) e]
        ParseOk modLoad -> do
          let (dir, base) = splitFileName fn
              cdir        = dir </> ".drhaskell"
              cfn         = cdir </> base
          liftIO $ createDirectoryIfMissing False cdir

          Just level <- foldr (liftM2 mplus) (return $ Just Level1)
                        [use forceLevel, return $ determineLevel modLoad]
          (transModule, transErrors) <- transformModuleS modLoad
          liftIO $ writeFile cfn $ printModified transModule

          checkErrors <- liftIO $ runCheckLevel level fn
          let errors = checkErrors ++ transErrors
          if null errors || nonstrict
            then let es   = map CheckError errors
                     dm e = DirectMessage $ displayException e
                     handler' e = return $ es ++ [dm e]
                 in MC.handleAll handler' $ do
                      liftInterpreter $ loadModules [cfn]
                      liftInterpreter $ setTopLevelModules ["Main"]
                      liftRepl $ modify $ Control.Lens.set filename fn
                      rt <- use runTests
                      testErrors <- if rt then runAllTests else return []
                      return $ es ++ map DirectMessage testErrors
            else
              return $ map CheckError errors

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

runAllTests :: Repl [String]
runAllTests = MC.handleAll (\_ -> return []) $
  liftInterpreter (interpret "runAllTests" (as :: IO [String])) >>= liftIO

duplPrelImps :: [Error l] -> [String]
duplPrelImps [] = []
duplPrelImps (e:es) = case e of
                       Duplicated name _ -> nameString name : duplPrelImps es
                       _                 -> duplPrelImps es

addMyPrelude :: ModifiedModule -> ModifiedModule
addMyPrelude = addImport ImportDecl {importAnn = (), importModule = ModuleName () "MyPrelude", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing} . addImport ImportDecl {importAnn = (), importModule = ModuleName () "Prelude", importQualified = True, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}

transformModule :: MonadIO m => ReplState -> ModifiedModule -> m (ModifiedModule, [Error SrcSpanInfo])
transformModule s m = do
  (m', es) <- liftIO $ Tee.transformModule m
  if s ^. customPrelude
  then return (addMyPrelude m', es)
  else return (m', es)

transformModuleS :: (MonadIO m, MonadState ReplState m) => ModifiedModule -> m (ModifiedModule, [Error SrcSpanInfo])
transformModuleS m = MS.get >>= flip transformModule m
