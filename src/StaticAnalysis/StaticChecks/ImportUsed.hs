-- | Check if a module is imported
module StaticAnalysis.StaticChecks.ImportUsed (
  importUsed
) where

import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors

-- | Checks if a module has import declarations
importUsed :: Module l -> [Error l]
importUsed (Module _ _ _ imports _) =
  map (\(ImportDecl _ n _ _ _ _ _ _) -> Imported n) imports
importUsed _ = []
