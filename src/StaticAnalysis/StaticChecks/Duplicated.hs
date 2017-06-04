module StaticAnalysis.StaticChecks.Duplicated where

import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors
import           StaticAnalysis.StaticChecks.Select


--------------------------------------------------------------------------------
-- Duplicated name in imported module

duplicated :: Eq l => Module l -> [Module l] -> [Error l]
duplicated _ [] = []
duplicated m (m':ms) =
  [Duplicated n (nameOfModule m')
  | n <- defNames m, nameString n `elem` defNameStrs m'] ++ duplicated m ms
  where defNameStrs m = map nameString $ defNames m
