module StaticAnalysis.StaticChecks.TypeVarApplication where


import           AstChecks.Check
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors

typeVarApplication :: Module l -> [Error l]
typeVarApplication (Module _ _ _ _ decls) = map TypeVarApplication $
  mapOverTypes checkForTypeVarApp decls

checkForTypeVarApp :: Type l -> [Name l]
checkForTypeVarApp (TyApp _ (TyVar _ name) _) = [name]
checkForTypeVarApp t = mapOverTyTypesRec False checkForTypeVarApp [t]
