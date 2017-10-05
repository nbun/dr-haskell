module StaticAnalysis.StaticChecks.Record (recordUsed) where

import Data.Maybe                           (mapMaybe)
import Control.Monad                        (mplus)
import Language.Haskell.Exts                --(Decl (..), ConDecl (..), QualConDecl (..), Module (..))
import StaticAnalysis.Messages.StaticErrors (Error (..))

-- | Checks if a data declaration is used
recordUsed :: Module l -> [Error l]
recordUsed (Module _ _ _ _ decls) = map RecordUsed $ mapMaybe recordLoc decls
recordUsed _ = []

recordLoc :: Decl l -> Maybe l
recordLoc decl =
  case decl of
    DataDecl _ _ _ _ qcds _  -> recDecls qcds
    DataInsDecl _ _ _ qcds _ -> recDecls qcds
    _                        -> Nothing

  where recDecls = foldr (mplus . recDeclLoc) Nothing
        recDeclLoc (QualConDecl _ _ _ (RecDecl l _ _)) = Just l
        recDeclLoc _                                   = Nothing

test = Module () Nothing [] [] [DataDecl () (DataType ()) Nothing (DHead () (Ident () "Test")) [QualConDecl () Nothing Nothing (RecDecl () (Ident () "Test") [FieldDecl () [Ident () "asd"] (TyCon () (UnQual () (Ident () "Int")))])] Nothing]
