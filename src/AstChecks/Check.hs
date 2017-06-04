module AstChecks.Check where

import           Control.Monad
import           Language.Haskell.Exts

type ImportDeclCheck l a = ImportDecl l -> [a]
type DeclCheck l a       = Decl l -> [a]
type ExpCheck l a        = Exp l -> [a]
type TypeCheck l a       = Type l -> [a]
type ModuleCheck l a     = Module l -> [a]

cId _ = []

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
    concatMap idcF importdecls
    ++ concatMap dcF decls
    ++ mapOverDeclsNew dcF ecF decls
    ++ mapOverTypes tcF decls

checkASTv2 :: ImportDeclCheck l a -> DeclCheck l a -> ExpCheck l a -> TypeCheck l a -> ModuleCheck l a -> Module l -> [a]
checkASTv2 idcF dcF ecF tcF mcF m@(Module srcInfo modulehead modulepragmas importdecls decls) =
    checkAST idcF dcF ecF tcF m
    ++ mcF m

mapOverDecls :: ExpCheck l a -> [Decl l] -> [a]
mapOverDecls = mapOverDeclsNew (const [])

mapOverDeclsNew :: DeclCheck l a -> ExpCheck l a -> [Decl l] -> [a]
mapOverDeclsNew _ _ [] = []
mapOverDeclsNew dcF ecF (x:xs) = --TODO: use dcF on all cases
    case x of
        SpliceDecl _ e        -> ecF e
                                 ++ mapOverExpNew dcF ecF e
                                 ++ mapOverDeclsNew dcF ecF xs
        FunBind _ matches     -> mapOverFunBindNew dcF ecF matches
                                 ++ mapOverDeclsNew dcF ecF xs
        PatBind _ _ rhs mbind -> mapOverRhsNew dcF ecF rhs
                                 ++ mapOverDeclsNew dcF ecF xs
                                 ++ mapOverMaybeBindsNew dcF ecF mbind
        _                     -> mapOverDeclsNew dcF ecF xs

mapOverFunBind :: ExpCheck l a -> [Match l] -> [a]
mapOverFunBind = mapOverFunBindNew (const [])

mapOverFunBindNew :: DeclCheck l a -> ExpCheck l a -> [Match l] -> [a]
mapOverFunBindNew _ _ []         = []
mapOverFunBindNew dcF ecF (x:xs) =
    case x of
        Match _ _ _ rhs mbind        -> mapOverRhsNew dcF ecF rhs
                                        ++ mapOverMaybeBindsNew dcF ecF mbind
        InfixMatch _ _ _ _ rhs mbind -> mapOverRhsNew dcF ecF rhs
                                        ++ mapOverMaybeBindsNew dcF ecF mbind
    ++ mapOverFunBindNew dcF ecF xs

mapOverRhs :: ExpCheck l a -> Rhs l -> [a]
mapOverRhs = mapOverRhsNew (const [])

mapOverRhsNew :: DeclCheck l a -> ExpCheck l a -> Rhs l -> [a]
mapOverRhsNew dcF ecF (UnGuardedRhs _ e)    = ecF e
                                              ++ mapOverExpNew dcF ecF e
                                              ++ mapOverExpNew dcF ecF e
mapOverRhsNew dcF ecF (GuardedRhss _ gRhss) = concatMap (mapOverGuardedRhsNew dcF ecF) gRhss

mapOverGuardedRhs :: ExpCheck l a -> GuardedRhs l -> [a]
mapOverGuardedRhs = mapOverGuardedRhsNew (const [])

mapOverGuardedRhsNew :: DeclCheck l a -> ExpCheck l a -> GuardedRhs l -> [a]
mapOverGuardedRhsNew dcF ecF (GuardedRhs _ stmts e) = mapOverStmtsNew dcF ecF stmts
                                                      ++ ecF e
                                                      ++ mapOverExpNew dcF ecF e

mapOverStmts :: ExpCheck l a -> [Stmt l] -> [a]
mapOverStmts = mapOverStmtsNew (const [])

mapOverStmtsNew :: DeclCheck l a -> ExpCheck l a -> [Stmt l] -> [a]
mapOverStmtsNew _ _ []         = []
mapOverStmtsNew dcF ecF (x:xs) =
    case x of
        (Generator _ _ e) -> ecF e
                             ++ mapOverExpNew dcF ecF e
                             ++ mapOverStmtsNew dcF ecF xs
        (Qualifier _ e)   -> ecF e
                             ++ mapOverExpNew dcF ecF e
                             ++ mapOverStmtsNew dcF ecF xs
        (LetStmt _ bind)  -> mapOverBindsNew dcF ecF [bind]
                             ++ mapOverStmtsNew dcF ecF xs
        (RecStmt _ stmts) -> mapOverStmtsNew dcF ecF stmts
                             ++ mapOverStmtsNew dcF ecF xs

mapOverBinds :: ExpCheck l a -> [Binds l] -> [a]
mapOverBinds = mapOverBindsNew (const [])

mapOverBindsNew :: DeclCheck l a -> ExpCheck l a -> [Binds l] -> [a]
mapOverBindsNew _ _ []         = []
mapOverBindsNew dcF ecF (x:xs) =
    case x of
        (BDecls _ decls)    -> concatMap dcF decls
                               ++ mapOverDeclsNew dcF ecF decls
        (IPBinds _ ipbinds) -> foldr (\(IPBind _ _ e) ips -> ecF e
                                                             ++ mapOverExpNew dcF ecF e ++ ips)
                                     [] ipbinds
     ++ mapOverBindsNew dcF ecF xs

mapOverMaybeBinds :: ExpCheck l a -> Maybe (Binds l) -> [a]
mapOverMaybeBinds = mapOverMaybeBindsNew (const [])

mapOverMaybeBindsNew :: DeclCheck l a -> ExpCheck l a -> Maybe (Binds l) -> [a]
mapOverMaybeBindsNew dcF ecF (Just bind) = mapOverBindsNew dcF ecF [bind]
mapOverMaybeBindsNew _ _   Nothing       = []

mapOverTypes :: TypeCheck l a -> [Decl l] -> [a]
mapOverTypes _ [] = []
mapOverTypes tcF (x:xs) =
    case x of
        TypeDecl _ _ t                     -> tcF t
                                              ++ mapOverTypes tcF xs
        TypeInsDecl _ t1 t2                -> tcF t1
                                              ++ tcF t2
                                              ++ mapOverTypes tcF xs
        ClosedTypeFamDecl _ _ _ _ typeEqns -> foldr (\(TypeEqn _ t1 t2) ts -> tcF t1
                                                                              ++ tcF t2
                                                                              ++ ts)
                                                    [] typeEqns
                                              ++ mapOverTypes tcF xs
        DataInsDecl _ _ t _ _              -> tcF t
                                              ++ mapOverTypes tcF xs
        GDataInsDecl _ _ t _ _ _           -> tcF t
                                              ++ mapOverTypes tcF xs
        DefaultDecl _ ts                   -> concatMap tcF ts
                                              ++ mapOverTypes tcF xs
        TypeSig _ _ t                      -> tcF t
                                              ++ mapOverTypes tcF xs
        PatSynSig _ _ _ _ _ t              -> tcF t
                                              ++ mapOverTypes tcF xs
        ForImp _ _ _ _ _ t                 -> tcF t
                                              ++ mapOverTypes tcF xs
        ForExp _ _ _ _ t                   -> tcF t
                                              ++ mapOverTypes tcF xs
        SpecSig _ _ _ t                    -> concatMap tcF t
                                              ++ mapOverTypes tcF xs
        SpecInlineSig _ _ _ _ t            -> concatMap tcF t
                                              ++ mapOverTypes tcF xs
        _                                  -> mapOverTypes tcF xs

mapOverExpNew :: DeclCheck l a -> ExpCheck l a -> Exp l -> [a]
mapOverExpNew = mapOverExpRecNew True

mapOverExp :: ExpCheck l a -> Exp l -> [a]
mapOverExp = mapOverExpNew (const [])

mapOverExpRec :: Bool -> ExpCheck l a -> Exp l -> [a]
mapOverExpRec rec = mapOverExpRecNew rec (const [])

mapOverExpRecNew :: Bool -> DeclCheck l a -> ExpCheck l a -> Exp l -> [a]
mapOverExpRecNew rec dcF ecF e =
    case e of
        (InfixApp _ e1 _ e2)            -> ecF e1
                                           ++ ecF e2
                                           ++ mapOverExpRecNew rec dcF ecF e1
                                           ++ mapOverExpRecNew rec dcF ecF e2
        (App _ e1 e2)                   -> ecF e1
                                           ++ ecF e2
                                           ++ mapOverExpRecNew rec dcF ecF e1
                                           ++ mapOverExpRecNew rec dcF ecF e2
        (NegApp _ e)                    -> ecF e
                                           ++ mapOverExpRecNew rec dcF ecF e
        (Lambda _ _ e)                  -> ecF e
                                           ++ mapOverExpRecNew rec dcF ecF e
        (Let _ bs e)                    -> ecF e
                                           ++ mapOverExpRecNew rec dcF ecF e
                                           ++ mapOverBindsNew dcF ecF [bs]
        (If _ e1 e2 e3)                 -> ecF e1
                                           ++ ecF e2
                                           ++ ecF e3
                                           ++ mapOverExpRecNew rec dcF ecF e1
                                           ++ mapOverExpRecNew rec dcF ecF e2
                                           ++ mapOverExpRecNew rec dcF ecF e3
        (MultiIf _ gRhss)               -> foldr (\x xs -> mapOverGuardedRhsNew dcF ecF x
                                                           ++ xs)
                                                 [] gRhss
        (Case _ e alts)                 -> ecF e
                                           ++ mapOverExpRecNew rec dcF ecF e
                                           ++ foldr (\(Alt _ _ rhs mBinds) xs -> mapOverRhsNew dcF ecF rhs
                                                                                 ++ case mBinds of
                                                                                        Nothing -> []
                                                                                        Just bind -> mapOverBindsNew dcF ecF [bind])
                                                    [] alts
        (Do _ stmts)                    -> mapOverStmtsNew dcF ecF stmts
        (MDo _ stmts)                   -> mapOverStmtsNew dcF ecF stmts
        (Tuple _ _ exps)                -> foldr (\x xs -> mapOverExpRecNew rec dcF ecF x
                                                           ++ xs)
                                                 [] exps
        (TupleSection _ _ mExps)        -> foldr (\x xs -> case x of
                                                            Nothing -> []
                                                            Just e  -> ecF e
                                                                       ++ mapOverExpRecNew rec dcF ecF e
                                                                       ++ xs)
                                                 [] mExps
        (List _ exps)                   -> foldr (\x xs -> mapOverExpRecNew rec dcF ecF x
                                                           ++ xs)
                                                 [] exps
        (ParArray _ exps)               -> foldr (\x xs -> mapOverExpRecNew rec dcF ecF x
                                                           ++ xs)
                                                 [] exps
        (Paren _ e)                     -> ecF e
                                           ++ mapOverExpRecNew rec dcF ecF e
        (LeftSection _ e _)             -> ecF e
                                           ++ mapOverExpRecNew rec dcF ecF e
        (RightSection _ _ e)            -> ecF e
                                           ++ mapOverExpRecNew rec dcF ecF e
        (RecConstr _ _ fieldUpdates)    -> mapOverFieldsUpdates ecF fieldUpdates
        (RecUpdate _ e fieldUpdates)    -> ecF e
                                           ++ mapOverExpRecNew rec dcF ecF e
                                           ++ mapOverFieldsUpdates ecF fieldUpdates
        (EnumFrom _ e)                  -> ecF e
                                           ++ mapOverExpRecNew rec dcF ecF e
        (EnumFromTo _ e1 e2)            -> ecF e1
                                           ++ ecF e2
                                           ++ mapOverExpRecNew rec dcF ecF e1
                                           ++ mapOverExpRecNew rec dcF ecF e2
        (EnumFromThen _ e1 e2)          -> ecF e1
                                           ++ ecF e2
                                           ++ mapOverExpRecNew rec dcF ecF e1
                                           ++ mapOverExpRecNew rec dcF ecF e2
        (EnumFromThenTo _ e1 e2 e3)     -> ecF e1
                                           ++ ecF e2
                                           ++ ecF e3
                                           ++ mapOverExpRecNew rec dcF ecF e1
                                           ++ mapOverExpRecNew rec dcF ecF e2
                                           ++ mapOverExpRecNew rec dcF ecF e3
        (ParArrayFromTo _ e1 e2)        -> ecF e1
                                           ++ ecF e2
                                           ++ mapOverExpRecNew rec dcF ecF e1
                                           ++ mapOverExpRecNew rec dcF ecF e2
        (ParArrayFromThenTo _ e1 e2 e3) -> ecF e1
                                           ++ ecF e2
                                           ++ ecF e3
                                           ++ mapOverExpRecNew rec dcF ecF e1
                                           ++ mapOverExpRecNew rec dcF ecF e2
                                           ++ mapOverExpRecNew rec dcF ecF e3
        (ListComp _ e qualStmts)        -> ecF e
                                           ++ mapOverExpRecNew rec dcF ecF e
                                           ++ mapOverQualStmts ecF qualStmts
        (ParComp _ e qualStmtss)        -> ecF e
                                           ++ mapOverExpRecNew rec dcF ecF e
                                           ++ foldr (\x xs -> mapOverQualStmtsNew dcF ecF x
                                                              ++ xs)
                                                    [] qualStmtss
        (ParArrayComp _ e qualStmtss)   -> ecF e
                                           ++ mapOverExpRecNew rec dcF ecF e
                                           ++ foldr (\x xs -> mapOverQualStmtsNew dcF ecF x
                                                              ++ xs)
                                                    [] qualStmtss
        (ExpTypeSig _ e _)              -> ecF e
                                           ++ mapOverExpRecNew rec dcF ecF e
        (BracketExp _ bracket)          -> mapOverBracket ecF bracket
        e                               -> if rec
                                           then ecF e
                                           else []

mapOverBracket :: ExpCheck l a -> Bracket l -> [a]
mapOverBracket ecF x =
    case x of
        (ExpBracket _ e)  -> ecF e
                             ++ mapOverExp ecF e
        (DeclBracket _ d) -> mapOverDecls ecF d
        _                 -> []

mapOverFieldsUpdates :: ExpCheck l a -> [FieldUpdate l] -> [a]
mapOverFieldsUpdates ecF = concatMap (\x -> case x of
                                                (FieldUpdate _ _ e) -> ecF e
                                                _                   -> [])

mapOverQualStmts :: ExpCheck l a -> [QualStmt l] -> [a]
mapOverQualStmts = mapOverQualStmtsNew (const [])

mapOverQualStmtsNew :: DeclCheck l a -> ExpCheck l a -> [QualStmt l] -> [a]
mapOverQualStmtsNew _ _ []         = []
mapOverQualStmtsNew dcF ecF (x:xs) =
    case x of
        (QualStmt _ stmt)      -> mapOverStmtsNew dcF ecF [stmt]
                                  ++ mapOverQualStmtsNew dcF ecF xs
        (ThenTrans _ e)        -> ecF e
                                  ++ mapOverExpNew dcF ecF e
        (ThenBy _ e1 e2)       -> ecF e1
                                  ++ ecF e2
                                  ++ mapOverExpNew dcF ecF e1
                                  ++ mapOverExpNew dcF ecF e2
        (GroupBy _ e)          -> ecF e
                                  ++ mapOverExpNew dcF ecF e
        (GroupUsing _ e)       -> ecF e
                                  ++ mapOverExpNew dcF ecF e
        (GroupByUsing _ e1 e2) -> ecF e1
                                  ++ ecF e2
                                  ++ mapOverExpNew dcF ecF e1
                                  ++ mapOverExpNew dcF ecF e2
