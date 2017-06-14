module StaticAnalysis.StaticChecks.ModuleHeadUsed where

import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors
import           StaticAnalysis.StaticChecks.Select

moduleHeadUsed :: Module l -> [Error l]
moduleHeadUsed (Module _ maymodhead _ _ _) =
  case maymodhead of
    Just (ModuleHead _ name _ _) -> [ModuleHeadUsed name]
    Nothing                      -> []
