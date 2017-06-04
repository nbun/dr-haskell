module StaticAnalysis.StaticChecks.TypeVarApplication where


import           AstChecks.Check
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors
import           StaticAnalysis.StaticChecks.Select

typeVarApplication :: Module l -> [Error l]
typeVarApplication (Module _ _ _ _ decls) = map TypeVarApplication $ mapOverTypes checkForTypeVarApp decls

-- TODO mapOverType
checkForTypeVarApp :: Type l -> [Name l]
checkForTypeVarApp (TyApp _ (TyVar _ name) _) = [name]
checkForTypeVarApp _                          = []

