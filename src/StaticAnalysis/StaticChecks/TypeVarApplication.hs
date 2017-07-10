-- | Check if a type variable is applied to something
module StaticAnalysis.StaticChecks.TypeVarApplication (
  typeVarApplication
) where

import           AstChecks.Check
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors

-- | Checks if a type variable is applied to something
typeVarApplication :: Module l -> [Error l]
typeVarApplication (Module _ _ _ _ decls) = map TypeVarApplication $
  mapOverTypes checkForTypeVarApp decls
typeVarApplication _ = []

checkForTypeVarApp :: Type l -> [Name l]
checkForTypeVarApp (TyApp _ (TyVar _ n) _) = [n]
checkForTypeVarApp t = mapOverTyTypesRec False checkForTypeVarApp [t]
