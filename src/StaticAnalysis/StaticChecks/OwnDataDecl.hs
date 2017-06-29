-- | Check if a data declaration is used
module StaticAnalysis.StaticChecks.OwnDataDecl (ownDataDecl) where

import           Data.Maybe
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors

-- | Checks if a data declaration is used
ownDataDecl :: Module l -> [Error l]
ownDataDecl (Module _ _ _ _ decls) = map OwnDataDecl $ mapMaybe dataDeclLoc decls
ownDataDecl _ = []

dataDeclLoc :: Decl l -> Maybe l
dataDeclLoc decl =
  case decl of
    TypeDecl l _ _              -> Just l
    TypeFamDecl l _ _ _         -> Just l
    ClosedTypeFamDecl l _ _ _ _ -> Just l
    DataDecl l _ _ _ _ _        -> Just l
    GDataDecl l _ _ _ _ _ _     -> Just l
    DataFamDecl l _ _ _         -> Just l
    TypeInsDecl l _ _           -> Just l
    DataInsDecl l _ _ _ _       -> Just l
    GDataInsDecl l _ _ _ _ _    -> Just l
    _                           -> Nothing
