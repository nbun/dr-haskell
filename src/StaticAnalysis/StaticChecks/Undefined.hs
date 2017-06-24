module StaticAnalysis.StaticChecks.Undefined (module StaticAnalysis.StaticChecks.Undefined) where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors
import           StaticAnalysis.StaticChecks.Select

flip2 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip2 f b c a = f a b c


undef :: Eq l => Module l -> [Module l] -> [Error l]
undef m@(Module _ _ _ _ ds) ms = if impModsAsArg then concatMap undef' ds
                                                 else []
  where
    impMods      = map modName $ importedModules m
    argMods      = filter (/= "Prelude") $ map modName $ mapMaybe nameOfModule ms
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
