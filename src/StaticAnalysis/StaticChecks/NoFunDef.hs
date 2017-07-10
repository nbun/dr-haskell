-- | Check if a type signature misses an accompanying definition
module StaticAnalysis.StaticChecks.NoFunDef (
      noFunDef
) where

import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors
import           StaticAnalysis.StaticChecks.Select

-- | Checks if a type signature misses an accompanying definition
noFunDef :: Module l -> [Error l]
noFunDef m@Module{} = [NoFunDef sig (similar3 m (map fst . defNames) sig)
                      | sig <- sigNames, nameString sig `notElem` defStrs]
  where sigNames = concatMap (map fst . declName) $ typeSigs m
        defStrs  = map (nameString . fst) $ defFuncs m
noFunDef _ = []

