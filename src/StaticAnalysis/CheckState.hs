module StaticAnalysis.CheckState where

import StaticAnalysis.Messages.StaticErrors

import Control.Monad.State.Lazy
import Language.Haskell.Exts
import AstChecks.Check

import StaticAnalysis.StaticChecks.NoFunDef
import StaticAnalysis.StaticChecks.Undefined
import StaticAnalysis.StaticChecks.Duplicated
import StaticAnalysis.StaticChecks.TypeVarApplication

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

main :: IO ()
main = do
  m <- getAST "src/StaticAnalysis/StaticChecks/Test.hs"
  p <- fmap void $ getAST "src/StaticAnalysis/StaticChecks/Test.hs"
  n <- getAST "src/Repl/Main.hs"
  let test = do
        check noFunDef
        check undef
        checkExt duplicated [n]
        check typeVarApplication
  putStrLn $ prettyCheckState (execState test (CheckState m "" []))
  -- print p
