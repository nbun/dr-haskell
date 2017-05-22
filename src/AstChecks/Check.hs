module AstChecks.Check where

import           Control.Monad
import           Language.Haskell.Exts

type Response l = Maybe (String, l)

type ImportDeclCheck l a = ImportDecl l -> a
type DeclCheck l a = Decl l -> a
type ExpCheck l a = Exp l -> a
type TypeCheck l a = Type l -> a
type ModuleCheck l a = Module l -> [a]

checkId :: a -> Response l
checkId _ = Nothing

checkIdString :: a -> String
checkIdString _ = ""

getAST path = do
    (ParseOk ast) <- parseFile path
    return ast

runCheck :: FilePath -> ImportDeclCheck SrcSpanInfo a -> DeclCheck SrcSpanInfo a -> ExpCheck SrcSpanInfo a -> TypeCheck SrcSpanInfo a -> IO [a]
runCheck path idcF dcF ecF tcF =
    checkAST idcF dcF ecF tcF <$> getAST path

runCheckv2 :: FilePath -> ImportDeclCheck SrcSpanInfo a -> DeclCheck SrcSpanInfo a -> ExpCheck SrcSpanInfo a -> TypeCheck SrcSpanInfo a -> ModuleCheck SrcSpanInfo a -> IO [a]
runCheckv2 path idcF dcF ecF tcF mcF =
    checkASTv2 idcF dcF ecF tcF mcF <$> getAST path

-- TODO: check for missing patternmatchings
checkAST :: ImportDeclCheck l a -> DeclCheck l a -> ExpCheck l a -> TypeCheck l a -> Module l -> [a]
checkAST idcF dcF ecF tcF (Module srcInfo modulehead modulepragmas importdecls decls) =
    map idcF importdecls ++
    map dcF decls ++ -- TODO: add map over other decls like where and let
    mapOverDecls ecF decls ++
    mapOverTypes tcF decls

checkASTv2 :: ImportDeclCheck l a -> DeclCheck l a -> ExpCheck l a -> TypeCheck l a -> ModuleCheck l a -> Module l -> [a]
checkASTv2 idcF dcF ecF tcF mcF m@(Module srcInfo modulehead modulepragmas importdecls decls) =
    checkAST idcF dcF ecF tcF m ++ mcF m

mapOverDecls :: ExpCheck l a -> [Decl l] -> [a]
mapOverDecls _  [] = []
mapOverDecls ecF (x:xs) =
    case x of
        SpliceDecl _ e    -> ecF e : mapOverExp ecF e ++ mapOverDecls ecF xs
        FunBind _ matches -> mapOverFunBind ecF matches ++ mapOverDecls ecF xs
        PatBind _ _ rhs _ -> mapOverRhs ecF rhs ++ mapOverDecls ecF xs
        _                 -> mapOverDecls ecF xs

mapOverFunBind :: ExpCheck l a -> [Match l] -> [a]
mapOverFunBind _ [] = []
mapOverFunBind ecF (x:xs) =
    case x of
        Match _ _ _ rhs _        -> mapOverRhs ecF rhs
        InfixMatch _ _ _ _ rhs _ -> mapOverRhs ecF rhs
    ++ mapOverFunBind ecF xs


mapOverRhs :: ExpCheck l a -> Rhs l -> [a]
mapOverRhs ecF (UnGuardedRhs _ e) = ecF e : mapOverExp ecF e ++ mapOverExp ecF e
mapOverRhs ecF (GuardedRhss _ gRhss) = concatMap (mapOverGuardedRhs ecF) gRhss

mapOverGuardedRhs :: ExpCheck l a -> GuardedRhs l -> [a]
mapOverGuardedRhs ecF (GuardedRhs _ stmts e) = mapOverStmts ecF stmts ++ (ecF e : mapOverExp ecF e)

mapOverStmts :: ExpCheck l a -> [Stmt l] -> [a]
mapOverStmts _ [] = []
mapOverStmts ecF (x:xs) =
    case x of
        (Generator _ _ e) -> ecF e : mapOverExp ecF e ++ mapOverStmts ecF xs
        (Qualifier _ e)   -> ecF e : mapOverExp ecF e ++ mapOverStmts ecF xs
        (LetStmt _ bind)  -> mapOverBinds ecF [bind] ++ mapOverStmts ecF xs
        (RecStmt _ stmts) -> mapOverStmts ecF stmts ++ mapOverStmts ecF xs

mapOverBinds :: ExpCheck l a -> [Binds l] -> [a]
mapOverBinds _ [] = []
mapOverBinds ecF (x:xs) =
    case x of
        (BDecls _ decls)    -> mapOverDecls ecF decls
        (IPBinds _ ipbinds) -> foldr (\(IPBind _ _ e) ips -> ecF e : mapOverExp ecF e ++ ips) [] ipbinds
     ++ mapOverBinds ecF xs

mapOverTypes :: TypeCheck l a -> [Decl l] -> [a]
mapOverTypes _ [] = []
mapOverTypes tcF (x:xs) =
    case x of
        TypeDecl _ _ t                     -> tcF t : mapOverTypes tcF xs
        TypeInsDecl _ t1 t2                -> tcF t1 : tcF t2 : mapOverTypes tcF xs
        ClosedTypeFamDecl _ _ _ _ typeEqns -> foldr (\(TypeEqn _ t1 t2) ts -> tcF t1 : tcF t2 : ts) [] typeEqns ++ mapOverTypes tcF xs
        DataInsDecl _ _ t _ _              -> tcF t : mapOverTypes tcF xs
        GDataInsDecl _ _ t _ _ _           -> tcF t : mapOverTypes tcF xs
        DefaultDecl _ ts                   -> map tcF ts ++ mapOverTypes tcF xs
        TypeSig _ _ t                      -> tcF t : mapOverTypes tcF xs
        PatSynSig _ _ _ _ _ t              -> tcF t : mapOverTypes tcF xs
        ForImp _ _ _ _ _ t                 -> tcF t : mapOverTypes tcF xs
        ForExp _ _ _ _ t                   -> tcF t : mapOverTypes tcF xs
        SpecSig _ _ _ t                    -> map tcF t ++ mapOverTypes tcF xs
        SpecInlineSig _ _ _ _ t            -> map tcF t ++ mapOverTypes tcF xs
        _                                  -> mapOverTypes tcF xs

mapOverExp :: ExpCheck l a -> Exp l -> [a]
mapOverExp ecF e =
    case e of
        (InfixApp _ e1 _ e2)            -> ecF e1 : ecF e2 : mapOverExp ecF e1 ++ mapOverExp ecF e2
        (App _ e1 e2)                   -> ecF e1 : ecF e2 : mapOverExp ecF e1 ++ mapOverExp ecF e2
        (NegApp _ e)                    -> ecF e : mapOverExp ecF e
        (Lambda _ _ e)                  -> ecF e : mapOverExp ecF e
        (Let _ bs e)                    -> ecF e : mapOverExp ecF e ++ mapOverBinds ecF [bs]
        (If _ e1 e2 e3)                 -> ecF e1 : ecF e2 : ecF e3 : mapOverExp ecF e1 ++ mapOverExp ecF e2 ++ mapOverExp ecF e3
        (MultiIf _ gRhss)               -> foldr (\x xs -> mapOverGuardedRhs ecF x ++ xs) [] gRhss
        (Case _ e alts)                 -> ecF e : mapOverExp ecF e ++ foldr (\(Alt _ _ rhs mBinds) xs ->
                                                                                    mapOverRhs ecF rhs ++ case mBinds of
                                                                                                                Nothing -> []
                                                                                                                Just bind -> mapOverBinds ecF [bind]) [] alts
        (Do _ stmts)                    -> mapOverStmts ecF stmts
        (MDo _ stmts)                   -> mapOverStmts ecF stmts
        (Tuple _ _ exps)                -> foldr (\x xs -> mapOverExp ecF x ++ xs) [] exps
        (TupleSection _ _ mExps)        -> foldr (\x xs -> case x of
                                                            Nothing -> []
                                                            Just e  -> ecF e : mapOverExp ecF e ++ xs) [] mExps
        (List _ exps)                   -> foldr (\x xs -> mapOverExp ecF x ++ xs) [] exps
        (ParArray _ exps)               -> foldr (\x xs -> mapOverExp ecF x ++ xs) [] exps
        (Paren _ e)                     -> ecF e : mapOverExp ecF e
        (LeftSection _ e _)             -> ecF e : mapOverExp ecF e
        (RightSection _ _ e)            -> ecF e : mapOverExp ecF e
        (RecConstr _ _ fieldUpdates)    -> mapOverFieldsUpdates ecF fieldUpdates
        (RecUpdate _ e fieldUpdates)    -> ecF e : mapOverExp ecF e ++ mapOverFieldsUpdates ecF fieldUpdates
        (EnumFrom _ e)                  -> ecF e : mapOverExp ecF e
        (EnumFromTo _ e1 e2)            -> ecF e1 : ecF e2 : mapOverExp ecF e1 ++ mapOverExp ecF e2
        (EnumFromThen _ e1 e2)          -> ecF e1 : ecF e2 : mapOverExp ecF e1 ++ mapOverExp ecF e2
        (EnumFromThenTo _ e1 e2 e3)     -> ecF e1 : ecF e2 : ecF e3 : mapOverExp ecF e1 ++ mapOverExp ecF e2 ++ mapOverExp ecF e3
        (ParArrayFromTo _ e1 e2)        -> ecF e1 : ecF e2 : mapOverExp ecF e1 ++ mapOverExp ecF e2
        (ParArrayFromThenTo _ e1 e2 e3) -> ecF e1 : ecF e2 : ecF e3 : mapOverExp ecF e1 ++ mapOverExp ecF e2 ++ mapOverExp ecF e3
        (ListComp _ e qualStmts)        -> ecF e : mapOverExp ecF e ++ mapOverQualStmts ecF qualStmts
        (ParComp _ e qualStmtss)        -> ecF e : mapOverExp ecF e ++ foldr (\x xs -> mapOverQualStmts ecF x ++ xs) [] qualStmtss
        (ParArrayComp _ e qualStmtss)   -> ecF e : mapOverExp ecF e ++ foldr (\x xs -> mapOverQualStmts ecF x ++ xs) [] qualStmtss
        (ExpTypeSig _ e _)              -> ecF e : mapOverExp ecF e
        (BracketExp _ bracket)          -> mapOverBracket ecF bracket
        e                               -> [ecF e]

mapOverBracket :: ExpCheck l a -> Bracket l -> [a]
mapOverBracket ecF x =
    case x of
        (ExpBracket _ e)  -> ecF e : mapOverExp ecF e
        (DeclBracket _ d) -> mapOverDecls ecF d
        _                 -> []

mapOverFieldsUpdates :: ExpCheck l a -> [FieldUpdate l] -> [a]
mapOverFieldsUpdates ecF = concatMap (\x -> case x of
                                                (FieldUpdate _ _ e) -> [ecF e]
                                                _                   -> [])

mapOverQualStmts :: ExpCheck l a -> [QualStmt l] -> [a]
mapOverQualStmts _ [] = []
mapOverQualStmts ecF (x:xs) =
    case x of
        (QualStmt _ stmt)      -> mapOverStmts ecF [stmt] ++ mapOverQualStmts ecF xs
        (ThenTrans _ e)        -> ecF e : mapOverExp ecF e
        (ThenBy _ e1 e2)       -> ecF e1 : ecF e2 : mapOverExp ecF e1 ++ mapOverExp ecF e2
        (GroupBy _ e)          -> ecF e : mapOverExp ecF e
        (GroupUsing _ e)       -> ecF e : mapOverExp ecF e
        (GroupByUsing _ e1 e2) -> ecF e1 : ecF e2 : mapOverExp ecF e1 ++ mapOverExp ecF e2
