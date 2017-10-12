-- | Contains level definitions and functions to run checks comfortably
module StaticAnalysis.CheckState (runCheckLevel, Level(..)) where

import AstChecks.Check                                (cId, checkAST,
                                                       checkASTv2, getAST)
import Control.Monad.Catch                            (handleAll)
import Control.Monad.State.Lazy                       (State, StateT, execState,
                                                       get, put)
import Data.Functor.Identity                          (Identity)
import Language.Haskell.Exts                          (Module, SrcSpanInfo)
import Paths_drhaskell                                (getDataFileName)
import StaticAnalysis.Level                           (Level (..))
import StaticAnalysis.Messages.StaticErrors           (Error)
import StaticAnalysis.StaticChecks.DoUsed             (doUsed)
import StaticAnalysis.StaticChecks.Duplicated         (duplicated)
import StaticAnalysis.StaticChecks.HigherOrder        (checkForHigherOrderFunction)
import StaticAnalysis.StaticChecks.ImportUsed         (importUsed)
import StaticAnalysis.StaticChecks.Lambda             (lambdaCheck)
import StaticAnalysis.StaticChecks.ModuleHeadUsed     (moduleHeadUsed)
import StaticAnalysis.StaticChecks.NoFunDef           (noFunDef)
import StaticAnalysis.StaticChecks.NoTypeDef          (noTypeDef)
import StaticAnalysis.StaticChecks.OwnDataDecl        (ownDataDecl)
import StaticAnalysis.StaticChecks.Pragma             (pragmaCheck)
import StaticAnalysis.StaticChecks.Record             (recordUsed)
import StaticAnalysis.StaticChecks.Shadowing          (shadowing)
import StaticAnalysis.StaticChecks.TypeInstance       (typeinstanceCheck)
import StaticAnalysis.StaticChecks.TypeVarApplication (typeVarApplication)
import StaticAnalysis.StaticChecks.TypeVars           (checkForTypVar)
import StaticAnalysis.StaticChecks.Undefined          (undef)

-- | State that contains the current module and a list of errors
data CheckState l = CheckState (Module l) [Error l]

instance Show l => Show (CheckState l) where
  show (CheckState _ es) = unlines $ map show es

-- | Runs a check function and adds the result to the list of errors
check :: (Module l -> [Error l]) -> State (CheckState l) ()
check check = do
  CheckState m errors <- get
  put $ CheckState m (check m ++ errors)

-- | Runs a check function that accepts external input, such as imported modules
checkExt :: (Module l -> a -> [Error l]) -> a -> State (CheckState l) ()
checkExt check ext = do
  CheckState m errors <- get
  put $ CheckState m (check m ext ++ errors)

-- | A Level is a function that combines multiple checks by use of the
-- CheckState monad
type LevelT = [Module SrcSpanInfo] -> StateT (CheckState SrcSpanInfo) Identity ()

--------------------------------------------------------------------------------
-- Level definitions

{- Level 1
Implemented:
  ⊕ Imports verbieten
  ⊕ Module verbieten
  ⊕ Higher-Order und Lambda Funktionen verbieten
  ⊕ do Notation verbieten
  ⊕ Eigene Datentypen verbieten
  ⊕ Typsignaturen erzwingen
  ⊕ Hiding von Prelude Standardfunktionen (z.B.: map, reverse, list, >=, >> etc.)

TODO:
  ⊕ check-expect erzwingen
-}

levelOne :: LevelT
levelOne p = do
  check importUsed
  check moduleHeadUsed
  check ownDataDecl
  check noFunDef
  check typeVarApplication
  check doUsed
  check recordUsed
  checkExt undef p
  checkExt duplicated p
  check $ checkAST cId cId cId checkForHigherOrderFunction
  check $ checkAST cId cId lambdaCheck cId
  check $ checkASTv2 cId cId cId cId noTypeDef
  check $ checkAST cId shadowing cId cId
  check $ checkAST cId cId cId checkForTypVar
  check $ checkASTv2 cId cId cId cId pragmaCheck
  check $ checkASTv2 cId typeinstanceCheck cId cId cId

{- Level 2
Implemented:
  ⊖ Eigene Datentypen erlauben
  ⊕ Typvariablen verbieten
  ⊕ Showinstanzen für Datentypen generieren

TODO:
  ⊖ Hiding einiger Preludefunktionen aufheben (z.B.: list)
-}

levelTwo :: LevelT
levelTwo p = do
  check importUsed
  check moduleHeadUsed
  check noFunDef
  check typeVarApplication
  check doUsed
  check recordUsed
  checkExt undef p
  checkExt duplicated p
  check $ checkASTv2 cId cId cId cId noTypeDef
  check $ checkAST cId shadowing cId cId
  check $ checkASTv2 cId cId cId cId pragmaCheck
  check $ checkASTv2 cId typeinstanceCheck cId cId cId

{- Level 3
Implemented:
  ⊖ Imports erlauben
  ⊖ Typvariablen erlauben
  ⊖ Higher-Order und Lambda Funktionen erlauben
TODO:
  ⊖ Preludehiding deaktivieren
  ⊖ Showinstanzen für Datentypen nicht mehr generieren
  ⊖ check-expect in quick check überführen
-}
levelThree :: LevelT
levelThree p = do
  check noFunDef
  check typeVarApplication
  check doUsed
  check recordUsed
  checkExt undef p
  checkExt duplicated p
  check $ checkASTv2 cId cId cId cId noTypeDef
  check $ checkAST cId shadowing cId cId
  check $ checkASTv2 cId cId cId cId pragmaCheck
  check $ checkASTv2 cId typeinstanceCheck cId cId cId

levelFull :: LevelT
levelFull _ = return ()

-- | Maps Level to LevelT for easier usage
levelMapping :: Level -> LevelT
levelMapping l = case l of
                   Level1    -> levelOne
                   Level2    -> levelTwo
                   Level3    -> levelThree
                   LevelFull -> levelFull

-- | Runs the checks determined by the given level on a Haskell file and returns
--   a list of errors
runCheckLevel :: Level -> FilePath -> IO [Error SrcSpanInfo]
runCheckLevel level path = do
  m <- getAST path
  mods <- handleAll (\_ -> return []) $ do
                myPrelPath <- getDataFileName "TargetModules/MyPrelude.hs"
                myPrel <- getAST myPrelPath
                return [myPrel]
  let (CheckState _ errors) = execState (levelMapping level mods)
                                        (CheckState m [])
  return errors
