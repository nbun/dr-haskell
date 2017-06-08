module StaticAnalysis.StaticChecks.Shadowing where

import           AstChecks.Check
import           Data.Set
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors
import           StaticAnalysis.StaticChecks.Select
import           StaticAnalysis.Messages.Prettify
import           Text.JSON


shadowing :: DeclCheck SrcSpanInfo (Error SrcSpanInfo)
shadowing p@(FunBind _ matches) = concatMap checkMatch matches
shadowing _                     = []

checkMatch :: Match SrcSpanInfo -> [Error SrcSpanInfo]
checkMatch (Match _ _ patterns body _)   = extractNameAndSearchBody patterns body
checkMatch (InfixMatch _ p1 _ p2 body _) = extractNameAndSearchBody (p1 : p2) body

extractNameAndSearchBody :: [Pat SrcSpanInfo] -> Rhs SrcSpanInfo -> [Error SrcSpanInfo]
extractNameAndSearchBody pats body =
    let names = extractName pats
    in mkUniq $ searchBodyForNames names body

extractName :: [Pat l] -> [String]
extractName []               = []
extractName (PVar _ name:ps) = nameString name : extractName ps
extractName (_:ps)           = extractName ps

searchBodyForNames :: [String] -> Rhs l -> [Error l]
searchBodyForNames names = mapOverRhsNew cId (checkExpForNames names)

checkExpForNames :: [String] -> ExpCheck l (Error l)
checkExpForNames names e =
    case e of
        (Var i qname) -> [Shadowing qname | searchOnQName names qname]
        _             -> []

searchOnQName :: [String] -> QName l -> Bool
searchOnQName names qname = getNameOfQName qname `elem` names

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList
