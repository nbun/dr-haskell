-- | Check if a module head is used
module StaticAnalysis.StaticChecks.ModuleHeadUsed (moduleHeadUsed) where

import Language.Haskell.Exts                (Module (..), ModuleHead (..))
import StaticAnalysis.Messages.StaticErrors (Error (..))

-- | Checks if a module head is used
moduleHeadUsed :: Module l -> [Error l]
moduleHeadUsed (Module _ maymodhead _ _ _) =
  case maymodhead of
    Just (ModuleHead _ n _ _) -> [ModuleHeadUsed n]
    Nothing                   -> []
moduleHeadUsed _ = []
