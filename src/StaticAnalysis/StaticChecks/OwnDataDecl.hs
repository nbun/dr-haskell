module StaticAnalysis.StaticChecks.OwnDataDecl (module StaticAnalysis.StaticChecks.OwnDataDecl) where

import           Data.Maybe
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors
import           StaticAnalysis.StaticChecks.Select

ownDataDecl :: Module l -> [Error l]
ownDataDecl (Module _ _ _ _ decls) = map OwnDataDecl $ mapMaybe dataDeclLoc decls


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
