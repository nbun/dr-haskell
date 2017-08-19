-- | Check if a module contains duplicated definitions
module StaticAnalysis.StaticChecks.Duplicated (duplicated) where

import Control.Monad                        (guard, mplus)
import Data.Maybe                           (isJust, mapMaybe)
import Language.Haskell.Exts                (ExportSpec (..),
                                             ExportSpecList (..), Module (..),
                                             ModuleHead (..), Name, QName)
import StaticAnalysis.Messages.StaticErrors (Error (..))
import StaticAnalysis.StaticChecks.Select   (defNames, nameOfModule, nameString,
                                             qNameName)

-- | Checks if a module defines a function or data type that is already imported
duplicated :: Module l -> [Module l] -> [Error l]
duplicated m ms = do
  (n, e) <- defNames m
  let m' = definedIn n ms
  guard $ isJust m'
  let Just mname = m'
  return $ Duplicated n e (nameOfModule mname)

-- | Returns a module, if the given name is already defined in the module
definedIn :: Name l -> [Module l] -> Maybe (Module l)
definedIn _ []     = Nothing
definedIn n (m:ms) = defined n m `mplus` definedIn n ms
 where defined n' m' = if nameString n' `elem` exported m'
                       then Just m'
                       else Nothing
       exported m'  = map (nameString .qNameName) $
                         mapMaybe exportSpecQName (exports m')

-- | Returns the list of exported entities of a module
exports :: Module l -> [ExportSpec l]
exports (Module _ (Just (ModuleHead _ _ _ (Just (ExportSpecList _ especs)))) _ _ _)
  = especs
exports _ = []

-- | Returns the qualified name of an ExportSpec if possible
exportSpecQName :: ExportSpec l -> Maybe (QName l)
exportSpecQName e = case e of
                      EVar _ qn           -> Just qn
                      EAbs _ _ qn         -> Just qn
                      EThingWith _ _ qn _ -> Just qn
                      _                   -> Nothing
