-- | Check if an entity is not defined
module StaticAnalysis.StaticChecks.Undefined (undef) where

import Control.Monad                        (guard)
import Data.List                            (nub)
import Data.Maybe                           (mapMaybe)
import Language.Haskell.Exts                (Module (..))
import StaticAnalysis.Messages.StaticErrors (Error (..))
import StaticAnalysis.StaticChecks.Select

-- | Checks if an entity is not defined in the module or the given imported
--   modules
undef :: Eq l => Module l -> [Module l] -> [Error l]
undef m@(Module _ _ _ _ ds) ms = if impModsAsArg then concatMap undef' ds
                                                 else []
  where
    impMods      = map modName $ importedModules m
    argMods      = filter (/= "Prelude") $ map modName $ mapMaybe nameOfModule
                                                                  ms
    impModsAsArg = all (`elem` argMods) impMods
    qns d        = nub $ qNamesOfExps (expsOfDecl d)
    defStrs d    = map nameString $ map fst (defNames m) ++ varsOfDecl d
    sims qn d    = similar3 d varsOfDecl (qNameName qn)
                   ++ similar3 m (map fst . defNames) (qNameName qn)
                   ++ concatMap (flip2 similar3 (map fst . defNames)
                                                (qNameName qn)) ms
    undef' d     = do
      qn <- qns d
      guard $ (nameString . qNameName) qn `notElem` defStrs d
      return $ Undefined (qNameName qn) (sims qn d)
undef _ _ = []

flip2 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip2 f b c a = f a b c

