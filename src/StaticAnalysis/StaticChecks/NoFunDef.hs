-- | Check if a type signature misses an accompanying definition
module StaticAnalysis.StaticChecks.NoFunDef (noFunDef) where

import Language.Haskell.Exts                (Module (..))
import StaticAnalysis.Messages.StaticErrors (Error (..))
import StaticAnalysis.StaticChecks.Select   (declName, defFuncs, defNames,
                                             nameString, similar3, typeSigs)

-- | Checks if a type signature misses an accompanying definition
noFunDef :: Module l -> [Error l]
noFunDef m@Module{} = [NoFunDef sig (similar3 m (map fst . defNames) sig)
                      | sig <- sigNames, nameString sig `notElem` defStrs]
  where sigNames = concatMap (map fst . declName) $ typeSigs m
        defStrs  = map (nameString . fst) $ defFuncs m
noFunDef _ = []
