module StaticAnalysis.CheckState where

import           AstChecks.Check
import           Control.Monad.Catch
import           Control.Monad.State.Lazy
import           Data.Functor.Identity
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors

import           StaticAnalysis.Messages.Prettify
import           StaticAnalysis.StaticChecks.DoUsed
import           StaticAnalysis.StaticChecks.Duplicated
import           StaticAnalysis.StaticChecks.HigherOrder
import           StaticAnalysis.StaticChecks.ImportUsed
import           StaticAnalysis.StaticChecks.Lambda
import           StaticAnalysis.StaticChecks.ModuleHeadUsed
import           StaticAnalysis.StaticChecks.NoFunDef
import           StaticAnalysis.StaticChecks.NoTypeDef
import           StaticAnalysis.StaticChecks.OwnDataDecl
import           StaticAnalysis.StaticChecks.Shadowing
import           StaticAnalysis.StaticChecks.TypeVarApplication
import           StaticAnalysis.StaticChecks.TypeVars
import           StaticAnalysis.StaticChecks.Undefined

data CheckState l = CheckState (Module l) [Error l]

instance Show l => Show (CheckState l) where
  show (CheckState _ es) = unlines $ map show es

prettyCheckState :: CheckState SrcSpanInfo -> String
prettyCheckState (CheckState _ es) =
  unlines $ map prettyError es

check :: (Module l -> [Error l]) -> State (CheckState l) ()
check check = do
  CheckState m errors <- get
  put $ CheckState m (check m ++ errors)

checkExt :: (Module l -> a -> [Error l]) -> a -> State (CheckState l) ()
checkExt check ext = do
  CheckState m errors <- get
  put $ CheckState m (check m ext ++ errors)


data Level = Level1 | Level2 | Level3 | LevelFull
  deriving Show

type LevelT = [Module SrcSpanInfo] -> StateT (CheckState SrcSpanInfo) Identity ()

{- Level 1
Implemented:
  ⊕ Imports verbieten
  ⊕ Module verbieten
  ⊕ Higher-Order und Lambda Funktionen verbieten
  ⊕ do Notation verbieten
  ⊕ Eigene Datentypen verbieten
  ⊕ Typsignaturen erzwingen

TODO:
  ⊕ check-expect erzwingen
  ⊕ Hiding von Prelude Standardfunktionen (z.B.: map, reverse, list, >=, >> etc.)
-}

levelOne :: LevelT
levelOne p = do
  check importUsed
  check moduleHeadUsed
  check ownDataDecl
  check noFunDef
  check typeVarApplication
  check doUsed
  checkExt undef [] -- [p]
  checkExt duplicated [] -- [p]
  check $ checkAST cId cId cId checkForHigherOrderFunction
  check $ checkAST cId cId lambdaCheck cId
  check $ checkASTv2 cId cId cId cId noTypeDef
  check $ checkAST cId shadowing cId cId
  check $ checkAST cId cId cId checkForTypVar

{- Level 2
Implemented:
  ⊖ Eigene Datentypen erlauben
  ⊕ Typvariablen verbieten

TODO:
  ⊕ Showinstanzen für Datentypen generieren
  ⊖ Hiding einiger Preludefunktionen aufheben (z.B.: list)
-}

levelTwo :: LevelT
levelTwo p = do
  check importUsed
  check moduleHeadUsed
  check noFunDef
  check typeVarApplication
  check doUsed
  checkExt undef [] -- [p]
  checkExt duplicated [] -- [p]
  check $ checkAST cId cId cId checkForHigherOrderFunction
  check $ checkAST cId cId lambdaCheck cId
  check $ checkASTv2 cId cId cId cId noTypeDef
  check $ checkAST cId shadowing cId cId
  check $ checkAST cId cId cId checkForTypVar

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
  check moduleHeadUsed
  check noFunDef
  check typeVarApplication
  check doUsed
  checkExt undef [] -- [p]
  checkExt duplicated [] -- [p]
  check $ checkASTv2 cId cId cId cId noTypeDef
  check $ checkAST cId shadowing cId cId

levelFull :: LevelT
levelFull p = return ()

levelMapping :: Level -> LevelT
levelMapping l = case l of
                   Level1    -> levelOne
                   Level2    -> levelTwo
                   Level3    -> levelThree
                   LevelFull -> levelFull

runCheckLevel :: Level -> FilePath -> IO [Error SrcSpanInfo]
runCheckLevel level path = do
  m <- getAST path
  -- p <- getAST "path/to/level/1/prelude.hs"
  let (CheckState _ errors) = execState (levelMapping level []) (CheckState m [])
  return errors
