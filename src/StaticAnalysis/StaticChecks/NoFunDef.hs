module StaticAnalysis.StaticChecks.NoFunDef (module StaticAnalysis.StaticChecks.NoFunDef) where

import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors
import           StaticAnalysis.StaticChecks.Select

noFunDef :: Module l -> [Error l]
noFunDef m@Module{} = [NoFunDef sig (similar3 m defNames sig)
                      | sig <- sigNames, nameString sig `notElem` defStrs]
  where sigNames = concatMap declName $ typeSigs m
        defStrs  = map nameString $ defNames m


