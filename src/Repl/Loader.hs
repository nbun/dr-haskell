{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Repl.Loader (
  LoadMessage(..),
  printLoadMessage,
  determineLevel,
  transformModule,
  loadModule,
  loadInitialModules
) where

import           Control.Lens                         hiding (Level)
import           Control.Monad.Catch                  as MC
import           Control.Monad.IO.Class
import           Control.Monad.State.Lazy             as MS
import           Language.Haskell.Interpreter
import           StaticAnalysis.StaticChecks.Select
import           System.Directory
import           System.FilePath

import           Language.Haskell.Exts                as Exts
import           Repl.Types
import           StaticAnalysis.CheckState
import           StaticAnalysis.Messages.Prettify
import           StaticAnalysis.Messages.StaticErrors
import qualified Testing.TestExpExtractor             as Tee
import           Util.ModifyAst

data LoadMessage = CheckError (Maybe Level) (Error SrcSpanInfo)
                 | DirectMessage String

printLoadMessage :: LoadMessage -> String
printLoadMessage (CheckError l e)  = prettyErrorForLintWithLevel l e
printLoadMessage (DirectMessage m) = m

--System.IO is needed for hFlush and stdout
--for proper putStrLn
loadInitialModules :: Repl ()
loadInitialModules = do
    Just level <- use forceLevel `mplusM` return (Just Level1)
    currentLevel .= level
    liftInterpreter $ setImportsQ [("Prelude", Nothing),
                                   ("System.IO", Just "System.IO")]
    case level of
         Level1 -> do
          liftInterpreter $ loadModules ["StartupEnvironment"]
          liftInterpreter $ setTopLevelModules ["StartupEnvironment"]
         otherwise -> return ()

  where
    mplusM = liftM2 mplus


--todo: better path handling

{-
loadModule does the following:
  1. it tries to parse the module. If this fails, it just returns the error
  2. it calculates the output path (in .drhaskell directory)
  3. it determines the level by (in this order) force options, the DrHaskell
     level pragma in the file or a default of currently 'Level 1'
  4. it runs the static checks that are associated with the selected level
  5. it transforms 'duplicate declaration' problems into a 'hiding' clause
     if the duplicates are from the prelude
  6. it writes the modified module to the determined path
  7. if there are no problems (or the nonstrict mode is used), it tries to
     actually load the module into the interpreter
  8. it runs the tests specified in the module (unless --no-test is used)
  9. it returns errors and problems that it encountered
-}

loadModule :: FilePath -> Repl [LoadMessage]
loadModule fname = MC.handleAll handler $ loadModule' fname
  where
    -- handles IO errors thrown by parseModified
    handler e = return [DirectMessage (displayException e)]
    loadModule' fn = do
      nonstrict <- use nonStrict
      pr <- liftIO $ parseModified fn
      case pr of
        ParseFailed l e ->
          return [CheckError Nothing $
                             SyntaxError (infoSpan (mkSrcSpan l l) []) e]
        ParseOk modLoad -> do
          let (dir, base) = splitFileName fn
              cdir        = dir </> ".drhaskell"
              cfn         = cdir </> base
          liftIO $ createDirectoryIfMissing False cdir

          Just level <- foldr (liftM2 mplus) (return $ Just Level1)
                        [use forceLevel, return $ determineLevel modLoad]

          checkErrors <- liftIO $ runCheckLevel level fn
          let (checkErrors', duplDecls) = duplPrelImps checkErrors
          (transModule, transErrors) <- transformModuleS duplDecls modLoad
          liftIO $ writeFile cfn $ printModified transModule

          let errors = checkErrors' ++ transErrors
          if null errors || nonstrict
            then let es   = map (CheckError (Just level)) errors
                     dm e = DirectMessage $ displayException e
                     handler' e = return $ es ++ [dm e]
                 in MC.handleAll handler' $ do
                      liftInterpreter $ loadModules [cfn]
                      liftInterpreter $ setTopLevelModules ["Main"]
                      liftRepl $ currentLevel .= level
                      liftRepl $ modify $ Control.Lens.set filename fn
                      rt <- use runTests
                      testErrors <- if rt then runAllTests else return []
                      return $ es ++ map DirectMessage testErrors
            else
              return $ map (CheckError (Just level)) errors

determineLevel :: ModifiedModule -> Maybe Level
determineLevel = foldr (mplus . extractLevel) Nothing . modifiedComments
  where
    extractLevel :: Comment -> Maybe Level
    extractLevel (Comment _ _ "# DRHASKELL LEVEL1 #")    = Just Level1
    extractLevel (Comment _ _ "# DRHASKELL LEVEL2 #")    = Just Level2
    extractLevel (Comment _ _ "# DRHASKELL LEVEL3 #")    = Just Level3
    extractLevel (Comment _ _ "# DRHASKELL LEVELFULL #") = Just LevelFull
    extractLevel (Comment _ _ "# DRHASKELL FULL #")      = Just LevelFull
    extractLevel _                                       = Nothing

runAllTests :: Repl [String]
runAllTests = MC.handleAll (\e -> return [displayException e]) $
  liftInterpreter (interpret "runAllTests" (as :: IO [String])) >>= liftIO


duplPrelImps :: [Error l] -> ([Error l], [ImportSpec l])
duplPrelImps []     = ([],[])
duplPrelImps (e:es) =
  case e of
    Duplicated name entity mod ->
      case (isMyPrelude mod, entity) of
        -- TODO add warning that a function/datatype was hidden
        (True, Function)   -> (es', ivar name : is)
        (True, Definition) -> (es', ivar name : is)
        (True, Datatype)   -> (es', IThingAll (namePos name) name : is)
        _                  -> (e:es', is)
    _ ->  (e:es', is)
  where (es', is) = duplPrelImps es
        ivar name = IVar (namePos name) name


isMyPrelude :: Maybe (Exts.ModuleName l) -> Bool
isMyPrelude (Just (ModuleName _ "MyPrelude")) = True
isMyPrelude _                                 = False

addMyPrelude :: [ImportSpec SrcSpanInfo] -> ModifiedModule -> ModifiedModule
addMyPrelude hideDefs = addImport ImportDecl
  {importAnn = noSrcSpan
  , importModule = ModuleName noSrcSpan "MyPrelude"
  , importQualified = False
  , importSrc = False
  , importSafe = False
  , importPkg = Nothing
  , importAs = Nothing
  , importSpecs = if not $ null hideDefs
                   then Just $ ImportSpecList noSrcSpan True hideDefs
                   else Nothing}
  . addImport ImportDecl
  {importAnn = noSrcSpan
  , importModule = ModuleName noSrcSpan "Prelude"
  , importQualified = True
  , importSrc = False
  , importSafe = False
  , importPkg = Nothing
  , importAs = Nothing
  , importSpecs = Nothing}
  . addImport ImportDecl
  {importAnn = noSrcSpan
  , importModule = ModuleName noSrcSpan "Prelude"
  , importQualified = False
  , importSrc = False
  , importSafe = False
  , importPkg = Nothing
  , importAs = Nothing
  , importSpecs = Just $ ImportSpecList
                           noSrcSpan
                           False
                           [IVar noSrcSpan $ Ident noSrcSpan "IO"]}

-- do the actual modifications to a module
-- currently this adds the 'runAllTests' declaration and a custom prelude
transformModule :: MonadIO m => [ImportSpec SrcSpanInfo] -> ReplState
                -> ModifiedModule -> m (ModifiedModule, [Error SrcSpanInfo])
transformModule hide s m = do
  (m', es) <- liftIO $ Tee.transformModule m
  if s ^. customPrelude
  then return
    (addMyPrelude
      hide $
      addDerivingToAllData
        "deriving (Prelude.Show, Prelude.Read, Prelude.Eq, Prelude.Ord)"
        m',
    es)
  else return (addDerivingToAllData "deriving (Show, Read, Eq, Ord)" m', es)

transformModuleS :: (MonadIO m, MonadState ReplState m) =>
                       [ImportSpec SrcSpanInfo]
                    -> ModifiedModule
                    -> m (ModifiedModule, [Error SrcSpanInfo])
transformModuleS hide m = MS.get >>= flip (transformModule hide) m
