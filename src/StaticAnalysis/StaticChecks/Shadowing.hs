module StaticAnalysis.StaticChecks.Shadowing where

import           AstChecks.Check
import           Language.Haskell.Exts
import           StaticAnalysis.StaticChecks.Select


shadowing :: DeclCheck l (Response l)
shadowing p@(FunBind _ matches) = concatMap checkMatch matches
shadowing _                     = []

checkMatch :: Match l -> [Response l]
checkMatch (Match _ _ patterns body _)   = extractNameAndSearchBody patterns body
checkMatch (InfixMatch _ p1 _ p2 body _) = extractNameAndSearchBody (p1 : p2) body

extractNameAndSearchBody :: [Pat l] -> Rhs l -> [Response l]
extractNameAndSearchBody pats body =
    let names = extractName pats
    in searchBodyForNames names body

extractName :: [Pat l] -> [String]
extractName []               = []
extractName (PVar _ name:ps) = nameString name : extractName ps
extractName (_:ps)           = extractName ps

searchBodyForNames :: [String] -> Rhs l -> [Response l]
searchBodyForNames names = mapOverRhsNew cId (checkExpForNames names)

checkExpForNames :: [String] -> ExpCheck l (Response l)
checkExpForNames names e =
    case e of
        (Var i qname) -> [Just ("Shadowing of " ++ getNameOfQName qname, i) | searchOnQName names qname]
        _             -> []

getNameOfQName :: QName l -> String
getNameOfQName (Qual _ _ name) = nameString name
getNameOfQName (UnQual _ name) = nameString name

searchOnQName :: [String] -> QName l -> Bool
searchOnQName names qname = getNameOfQName qname `elem` names
