{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Repl.Loader (
  LoadMessage(..),
  printLoadMessage,
  determineLevel,
  transformModule,
  loadModule,
  loadInitialModules,
  inferModule,
  initEmptyTI,
) where

import           Control.Lens                         hiding (Level)
import           Control.Monad.Catch                  as MC
import           Control.Monad.IO.Class
import           Control.Monad.State.Lazy             as MS
import           Data.Char                            (isDigit)
import           Data.List                            (findIndex, intercalate)
import           Data.List.Split                      (splitOn)
import           Data.Maybe
import           Language.Haskell.Interpreter
import           Paths_drhaskell
import           StaticAnalysis.StaticChecks.Select
import           System.Directory
import           System.FilePath

import           Language.Haskell.Exts                as Exts
import           Repl.Types
import           StaticAnalysis.CheckState
import           StaticAnalysis.Level                 (useOwnTI)
import           StaticAnalysis.Messages.Prettify
import           StaticAnalysis.Messages.StaticErrors
import qualified Testing.ArbitGen                     as AG
import qualified Testing.TestExpExtractor             as Tee
import qualified TypeInference.AbstractHaskell        as AH
import           TypeInference.Main
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
    ps <- liftIO initEmptyTI
    tiProg .= ps
    Just level <- use forceLevel `mplusM` return (Just Level1)
    currentLevel .= level
    liftInterpreter $ reset
    liftInterpreter $ setImportsQ [("Prelude", Nothing),
                                   ("System.IO", Just "System.IO")]
    case level of
         Level1 -> do
          liftInterpreter $ loadModules ["StartupEnvironment"]
          liftInterpreter $ setTopLevelModules ["StartupEnvironment"]
         _ -> return ()

  where
    mplusM = liftM2 mplus


--todo: better path handling

inferModule :: Exts.Module Exts.SrcSpanInfo
            -> IO (Either (TIError Exts.SrcSpanInfo) [AH.Prog Exts.SrcSpanInfo])
inferModule m
  = do datadir <- getDataDir
       let myPreludePath = datadir </> "TargetModules" </> "MyPrelude.hs"
       pre <- prelude myPreludePath
       return $ (:[pre]) <$> inferHSE [pre] m

initEmptyTI :: IO [AH.Prog Exts.SrcSpanInfo]
initEmptyTI = do
  datadir <- getDataDir
  let myPreludePath = datadir </> "TargetModules" </> "MyPrelude.hs"
  (:[]) <$> prelude myPreludePath

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
loadModule fname = MC.handleAll handler $ loadModule' $ adjustPath fname
  where
    -- handles IO errors thrown by parseModified
    handler e = return [DirectMessage $ displayException e]
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
          fl <- use forceLevel
          let levelSelectErrors = if checkLevelValid modLoad || isJust fl
                                  then []
                                  else [DirectMessage
                                        ("No valid level selection "++
                                         "found. Using Level 1")]

          checkErrors <- liftIO $ runCheckLevel level fn
          let (checkErrors', duplDecls) = duplPrelImps checkErrors
          (transModule, transErrors) <- transformModuleS duplDecls modLoad
          liftIO $ writeFile cfn $ printModified transModule

          tires <- liftIO $ inferModule (modifiedModule modLoad)
          let (tiErrors, tiprog) =
                case (useOwnTI level, tires) of
                  (False,      _)  -> ([], [])
                  (True,  Left e)  -> let pos = posOfTIError e
                                      in ([TypeError pos e], [])
                  (True,  Right p) -> ([], p)
          tiProg .= tiprog

          let errors' = checkErrors' ++ transErrors ++ if null checkErrors'
                                                       then tiErrors else []
              errors  = map (CheckError (Just level)) errors'

          if null errors || (nonstrict && not (any isCritical errors'))
            then let dm e = DirectMessage $ adjustGHCerror transModule $ displayException e
                     handler' e = do
                       loadInitialModules
                       return $ levelSelectErrors ++
                                           errors ++
                                           [dm e]
                 in MC.handleAll handler' $ do
                      liftInterpreter $ loadModules [cfn]
                      mods <- liftInterpreter getLoadedModules
                      liftInterpreter $ setTopLevelModules $ filter (/= "Tests") mods
                      liftRepl $ currentLevel .= level
                      liftRepl $ modify $ Control.Lens.set filename fn
                      rt <- use runTests
                      liftRepl $ promptModule .= determineModuleName transModule fname
                      testErrors <- if rt then runAllTests else return []
                      return $ levelSelectErrors ++
                               errors ++
                               map DirectMessage testErrors
            else do
              loadInitialModules
              return $ levelSelectErrors ++ errors
    --adjusts path for easier usage (appends .hs suffix)
    adjustPath :: FilePath -> FilePath
    adjustPath f = case reverse f of
                        's':'h':'.':_ -> f
                        _             -> f ++ ".hs"

checkLevelValid :: ModifiedModule -> Bool
checkLevelValid m = case filter isJust $
                         map commentToLevel $
                         modifiedComments m of
                    [_] -> True
                    _   -> False


determineLevel :: ModifiedModule -> Maybe Level
determineLevel m = mfilter (const $ checkLevelValid m) $
                   foldr (mplus . commentToLevel) Nothing $ modifiedComments m

determineModuleName :: ModifiedModule -> FilePath -> String
determineModuleName m fp = case modifiedModule m of
  (Module _ (Just (ModuleHead _ (ModuleName _ s) _ _)) _ _ _) -> s
  _ -> takeFileName fp


commentToLevel :: Comment -> Maybe Level
commentToLevel (Comment _ _ "# DRHASKELL LEVEL1 #")    = Just Level1
commentToLevel (Comment _ _ "# DRHASKELL LEVEL2 #")    = Just Level2
commentToLevel (Comment _ _ "# DRHASKELL LEVEL3 #")    = Just Level3
commentToLevel (Comment _ _ "# DRHASKELL LEVEL4 #")    = Just LevelFull
commentToLevel (Comment _ _ "# DRHASKELL LEVELFULL #") = Just LevelFull
commentToLevel (Comment _ _ "# DRHASKELL FULL #")      = Just LevelFull
commentToLevel _                                       = Nothing

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
                           [IVar noSrcSpan $ Ident noSrcSpan "IO",
                            IVar noSrcSpan $ Ident noSrcSpan "(==)",
                            IVar noSrcSpan $ Ident noSrcSpan "(/=)"]}

-- do the actual modifications to a module
-- currently this adds the 'runAllTests' declaration and a custom prelude
transformModule :: MonadIO m => [ImportSpec SrcSpanInfo] -> ReplState
                -> ModifiedModule -> m (ModifiedModule, [Error SrcSpanInfo])
transformModule hide s m = do
  (m', es) <- liftIO $ Tee.transformModule $ AG.generateArbitraryInModule m
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

adjustGHCerror :: ModifiedModule -> String -> String
adjustGHCerror m e = unlines $ map adjust $ lines e
  where
    adjust a@(' ':' ':' ':' ':_) = a
    adjust p                     = intercalate ":" $ adjNums $ splitOn ":" p
    adjNums xs = take i xs ++ adjNum (xs !! i) : drop (i+1) xs
      where
        Just i = findIndex (\n -> not (null n) && all isDigit n) xs
    adjNum n = show $ translateLine m $ read n
