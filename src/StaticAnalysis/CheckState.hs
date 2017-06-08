module StaticAnalysis.CheckState where

import StaticAnalysis.Messages.StaticErrors

import Control.Monad.State.Lazy
import Language.Haskell.Exts
import AstChecks.Check
import Control.Monad.Catch

import StaticAnalysis.StaticChecks.NoFunDef
import StaticAnalysis.StaticChecks.Undefined
import StaticAnalysis.StaticChecks.Duplicated
import StaticAnalysis.StaticChecks.TypeVarApplication
import StaticAnalysis.StaticChecks.HigherOrder
import StaticAnalysis.StaticChecks.Lambda
import StaticAnalysis.StaticChecks.NoTypeDef
import StaticAnalysis.StaticChecks.Shadowing
import StaticAnalysis.StaticChecks.TypeVars
import StaticAnalysis.Messages.Prettify

data CheckState l r = CheckState (Module l) r [Error l]

instance (Show r, Show l) => Show (CheckState l r) where
  show (CheckState _ r es) = unlines [show r, unlines $ map show es]

prettyCheckState :: Show r => CheckState SrcSpanInfo r -> String
prettyCheckState (CheckState _ r es) =
  unlines [show r, unlines $ map prettyError es]

check :: (Module l -> [Error l]) -> State (CheckState l r) ()
check check = do
  CheckState m result errors <- get
  put $ CheckState m result (check m ++ errors)

checkExt :: (Module l -> a -> [Error l]) -> a -> State (CheckState l r) ()
checkExt check ext = do
  CheckState m result errors <- get
  put $ CheckState m result (check m ext ++ errors)

runChecksL1 :: String -> IO [Error SrcSpanInfo]
runChecksL1 path = do
  m <- getAST path
  -- p <- getAST "path/to/level/1/prelude.hs"
  let checks = do
        check noFunDef
        check undef
        checkExt duplicated [] -- [p]
        check typeVarApplication
        check $ checkAST cId cId cId checkForHigherOrderFunction
        check $ checkAST cId cId lambdaCheck cId
        check $ checkASTv2 cId cId cId cId noTypeDef
        check $ checkAST cId shadowing cId cId
        check $ checkAST cId cId cId checkForTypVar
      (CheckState _ _ errors) = execState checks (CheckState m "" [])
  return errors
