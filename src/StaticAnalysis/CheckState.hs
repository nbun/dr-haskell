module StaticAnalysis.CheckState where

import StaticAnalysis.Messages.StaticErrors
import StaticAnalysis.StaticChecks.StaticChecks

import Control.Monad.State.Lazy
import Language.Haskell.Exts
import AstChecks.Check

data CheckState l r = CheckState (Module l) r [Error l]

instance (Show r, Show l) => Show (CheckState l r) where
  show (CheckState _ r es) = unlines [show r, show es]

check :: (Module l -> [Error l]) -> State (CheckState l r) ()
check check = do
  CheckState m result errors <- get
  put $ CheckState m result (check m ++ errors)

checkExt :: (Module l -> a -> [Error l]) -> a -> State (CheckState l r) ()
checkExt check ext = do
  CheckState m result errors <- get
  put $ CheckState m result (check m ext ++ errors)

-- test :: Eq l => State (CheckState l r) ()
-- test = do
--   check noFunDef
--   check undef
--   checkExt duplicated ["asd"]

main :: IO ()
main = do
  ast <- fmap void $ getAST "StaticAnalysis/StaticChecks/Test.hs"
  let test = do
        check noFunDef
        check undef
        checkExt duplicated [ast]
  print $ execState test (CheckState ast "" [])
