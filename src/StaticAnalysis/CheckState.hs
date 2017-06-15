module StaticAnalysis.CheckState where

import           StaticAnalysis.Messages.StaticErrors

import           AstChecks.Check
import           Control.Monad.Catch
import           Control.Monad.State.Lazy
import           Data.Functor.Identity
import           Language.Haskell.Exts

import           StaticAnalysis.Messages.Prettify
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
import           StaticAnalysis.StaticChecks.DoUsed

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

type Level a = a -> StateT (CheckState SrcSpanInfo) Identity ()

{- Level 1
⊕ Hiding von Prelude Standardfunktionen (z.B.: map, reverse, list, >=, >> etc.)
⊕ Imports verbieten
⊕ Module verbieten
⊕ Higher-Order und Lambda Funktionen verbieten
⊕ Eigene Datentypen verbieten
⊕ Typsignaturen erzwingen
⊕ check-expect erzwingen
⊕ do Notation verbieten
-}

levelOne :: Level a
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

runCheckLevel :: Level r -> FilePath -> IO [Error SrcSpanInfo]
runCheckLevel level path = do
  m <- getAST path
  -- p <- getAST "path/to/level/1/prelude.hs"
  let (CheckState _ errors) = execState (levelOne ()) (CheckState m [])
  return errors
