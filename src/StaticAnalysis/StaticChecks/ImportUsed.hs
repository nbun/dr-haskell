module StaticAnalysis.StaticChecks.ImportUsed where

import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors
import           StaticAnalysis.StaticChecks.Select


--------------------------------------------------------------------------------
-- Usage of imports

importUsed :: Module l -> [Error l]
importUsed (Module _ _ _ imports _) =
  map (\(ImportDecl _ name _ _ _ _ _ _) -> Imported name) imports
