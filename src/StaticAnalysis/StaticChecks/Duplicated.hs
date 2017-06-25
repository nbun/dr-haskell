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
 where defined n m = if nameString n `elem` exported m
                       then Just m
                       else Nothing
       exported m  = map (nameString .qNameName) $
                         mapMaybe exportSpecQName (exports m)

exports :: Module l -> [ExportSpec l]
exports (Module _ (Just (ModuleHead _ _ _ (Just (ExportSpecList _ especs)))) _ _ _)
  = especs
exports _ = []

exportSpecQName :: ExportSpec l -> Maybe (QName l)
exportSpecQName e = case e of
                      EVar _ qn           -> Just qn
                      EAbs _ _ qn         -> Just qn
                      EThingWith _ _ qn _ -> Just qn -- TODO constructor names?
                      _                   -> Nothing
