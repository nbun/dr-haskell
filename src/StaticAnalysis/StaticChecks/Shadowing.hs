module StaticAnalysis.StaticChecks.Shadowing (module StaticAnalysis.StaticChecks.Shadowing) where

import           AstChecks.Check
import           Data.List
import qualified Data.Set                             as DS
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.Prettify
import           StaticAnalysis.Messages.StaticErrors
import           StaticAnalysis.StaticChecks.Select
import           Text.JSON


shadowing :: DeclCheck SrcSpanInfo (Error SrcSpanInfo)
shadowing (FunBind _ matches) = concatMap checkMatch matches
shadowing _                   = []

checkMatch :: Match SrcSpanInfo -> [Error SrcSpanInfo]
checkMatch match = checkMatchWithAddNames match []

checkMatchWithAddNames :: Match SrcSpanInfo -> [String] -> [Error SrcSpanInfo]
checkMatchWithAddNames (Match _ _ patterns body _)   = extractNameAndSearchBodyWithAddNames patterns body
checkMatchWithAddNames (InfixMatch _ p1 _ p2 body _) = extractNameAndSearchBodyWithAddNames (p1 : p2) body

extractNameAndSearchBody :: [Pat SrcSpanInfo] -> Rhs SrcSpanInfo -> [Error SrcSpanInfo]
extractNameAndSearchBody pats body = extractNameAndSearchBodyWithAddNames pats body []

extractNameAndSearchBodyWithAddNames :: [Pat SrcSpanInfo] -> Rhs SrcSpanInfo -> [String] -> [Error SrcSpanInfo]
extractNameAndSearchBodyWithAddNames pats body ns =
    let names = ns ++ extractName pats
    in mkUniq $ searchBodyForNames names body

extractName :: [Pat SrcSpanInfo] -> [String]
extractName []               = []
extractName (PVar _ name:ps) = nameString name : extractName ps
extractName (_:ps)           = extractName ps

searchBodyForNames :: [String] -> Rhs SrcSpanInfo -> [Error SrcSpanInfo]
searchBodyForNames names = mapOverRhsNew cId (checkExpForNames names)

checkExpForNames :: [String] -> ExpCheck SrcSpanInfo (Error SrcSpanInfo)
checkExpForNames names e =
    case e of
        (Let _ binds exp) -> shadowingOnBinds binds exp names
        (Case _ _ alts)   -> shadowingOnAlts alts names
        _                 -> []

shadowingOnAlts :: [Alt SrcSpanInfo] -> [String] -> [Error SrcSpanInfo]
shadowingOnAlts (Alt li pat rhs _:alts) names =
    let newnames = extractName [pat]
    in case newnames of
        []  -> []
        [n] -> [Shadowing (UnQual li (Ident li n)) | n `elem` names]
        _   -> []
    ++ shadowingOnAlts alts names
shadowingOnAlts _ _ = []

shadowingOnBinds :: Binds SrcSpanInfo -> Exp SrcSpanInfo -> [String] -> [Error SrcSpanInfo]
shadowingOnBinds (BDecls _ (PatBind li pat rhs _:decls)) exp names =
    let newnames = extractName [pat]
        diff = names `intersect` newnames
        morenames = names ++ newnames
    in [Shadowing (UnQual li (Ident li n)) | n <- diff]
        ++ searchBodyForNames morenames rhs
        ++ mapOverDeclsWithAddNames decls names
shadowingOnBinds (IPBinds li ipbinds) exp names =
    matchAgainstIpBinds ipbinds names
shadowingOnBinds _ _ _ = []

mapOverDeclsWithAddNames :: [Decl SrcSpanInfo] -> [String] -> [Error SrcSpanInfo]
mapOverDeclsWithAddNames [] _ = []
mapOverDeclsWithAddNames (FunBind _ matches:decls) names =
    concatMap (`checkMatchWithAddNames` names) matches
    ++ mapOverDeclsWithAddNames decls names
mapOverDeclsWithAddNames (_:decls) names =
    mapOverDeclsWithAddNames decls names

matchAgainstIpBinds :: [IPBind SrcSpanInfo] -> [String] -> [Error SrcSpanInfo]
matchAgainstIpBinds [] _ = []
matchAgainstIpBinds (IPBind _ ipname _:binds) names =
    case ipname of
        (IPDup li n) -> [Shadowing (UnQual li (Ident li n)) | n `elem` names]
        (IPLin li n) -> [Shadowing (UnQual li (Ident li n)) | n `elem` names]

matchMatchesAgainstNames :: [Match SrcSpanInfo] -> [String] -> [Error SrcSpanInfo]
matchMatchesAgainstNames (m:matches) names =
    case m of
        (Match li _ pats rhs _)           ->
            let newnames = extractName pats
                diff = names `intersect` newnames
                morenames = names ++ newnames
            in [Shadowing (UnQual li (Ident li n)) | n <- diff]
                ++ searchBodyForNames morenames rhs
        (InfixMatch li pat _ pats rhs _ ) ->
            let newnames = extractName (pat:pats)
                diff = names `intersect` newnames
                morenames = names ++ newnames
            in [Shadowing (UnQual li (Ident li n)) | n <- diff]
                ++ searchBodyForNames morenames rhs
matchMatchesAgainstNames _ _ = []

searchOnQName :: [String] -> QName SrcSpanInfo -> Bool
searchOnQName names qname = getNameOfQName qname `elem` names

mkUniq :: Ord a => [a] -> [a]
mkUniq = DS.toList . DS.fromList
