module StaticAnalysis.StaticChecks.DoUsed (module StaticAnalysis.StaticChecks.DoUsed) where

import           AstChecks.Check
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors

-- | Checks if the Module contains any usage of the Do Statement
doUsed :: Eq l => Module l -> [Error l]
doUsed (Module _ _ _ _ decls) = mapOverDecls isDoExp decls

-- | The actual Do-Occurence-Check
isDoExp :: Exp l -> [Error l]
isDoExp (Do l _)  = [DoUsed l]
isDoExp (MDo l _) = [DoUsed l]
isDoExp _         = []
