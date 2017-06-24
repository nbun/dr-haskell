module StaticAnalysis.StaticChecks.Duplicated (module StaticAnalysis.StaticChecks.Duplicated) where

import           Control.Monad
import           Data.Maybe
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors
import           StaticAnalysis.StaticChecks.Select

--------------------------------------------------------------------------------
-- Duplicated name in imported module

-- duplicated :: Eq l => Module l -> [Module l] -> [Error l]
-- duplicated _ [] = []
-- duplicated m (m':ms) =
--   [Duplicated n (nameOfModule m')
--   | n <- defNames m, nameString n `elem` defNameStrs m'] ++ duplicated m ms
--   where defNameStrs m = map nameString $ defNames m

duplicated :: Eq l => Module l -> [Module l] -> [Error l]
duplicated m ms = do
  (n, e) <- defNames m
  let m' = definedIn n ms
  guard $ isJust m'
  let Just mname = m'
  return $ Duplicated n e (nameOfModule mname)

definedIn :: Name l -> [Module l] -> Maybe (Module l)
definedIn _ []     = Nothing
definedIn n (m:ms) = defined n m `mplus` definedIn n ms
 where defined n m = if nameString n `elem` defNameStrs m
                       then Just m
                       else Nothing
       defNameStrs m = map (nameString . fst) (defNames m)
