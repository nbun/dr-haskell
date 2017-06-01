import Language.Haskell.Exts
import Data.Functor
import AbstractHaskell

--- where modname: name of this module,
---       imports: list of modules names that are imported,
---       typedecls, opdecls, functions: see below
-- data Prog = Prog String [String] [TypeDecl] [FuncDecl] [OpDecl


astToAbstractHaskell :: (Module l) -> Prog
astToAbstractHaskell modu@(Module l modh mp imps declas) = undefined
  where moduleName = parseModuleHead modh
        typsignatures = parseTypeSignatur modu
          --parseTypeSignatur :: Module l -> [(Name l, Type l)]
--  where fundec = parseFunDecls declas
 --Prog moduleName
                                                                --(concatMap (parseImportList) imps)
                                                          -- (concatMap (parseTypeDecls moduleName typsignatures) declas
                                                                  --(parseFunDecls fundec)
                                                           --(parseOpDecls)
astToAbstractHaskell _ = Prog "" [] [] [] []

-- Hier noch für pattern definitionen
parseFunDecls :: String ->  [(Name l, Type l)] -> Decl l -> FuncDecl
parseFunDecls modulename typsignatures (FunBind l mas@(m:matches)) =
  Func "" (modulename, functionname) (parseArity mas) Public (buildType modulename (searchForType functionname typsignatures)) (parseRules mas)
    where functionname = parseMatchName m
parseFunDecls modulename typsignatures _ = Func "" (modulename,"") 0 Public Untyped (Rules [])

parseRules :: [Match l] -> Rules
parseRules ms = Rules (map parseRule ms)

parseRule :: Match l -> AbstractHaskell.Rule
parseRule (Match _ _ pats rhs _)        = parseRigthHands pats rhs
parseRule (InfixMatch _ pat1 _ pats rhs _) = parseRigthHands ([pat1] ++ pats) rhs

parseRigthHands :: [Pat l] -> Language.Haskell.Exts.Rhs l -> AbstractHaskell.Rule
parseRigthHands ps (UnGuardedRhs l e) = AbstractHaskell.Rule (map parsePatterns ps) (parseUnguarded e) []--parseLocalDef
parseRigthHands ps (GuardedRhss l gdrhs) = undefined--

parseUnguarded :: Exp l -> AbstractHaskell.Rhs
parseUnguarded e = SimpleRhs (parseExpr e)

parseExpr :: Exp l -> Expr
parseExpr (Language.Haskell.Exts.Var l qn)        = AbstractHaskell.Var (0,parseQName qn)-- varialben umgebung hinzufügen
parseExpr (Con l qn)                              = AbstractHaskell.Symbol ("",parseQName qn) -- modulname hinzufügen
parseExpr (Language.Haskell.Exts.Lit l lit)       = AbstractHaskell.Lit (parseLiteral lit)
parseExpr (InfixApp l exp1 qop exp2)              = InfixApply (parseExpr exp1) (parseQOp qop) (parseExpr exp2)
parseExpr (Language.Haskell.Exts.Lambda l pats e) = AbstractHaskell.Lambda (map parsePatterns pats) (parseExpr e)
parseExpr (Language.Haskell.Exts.Let l binds e)   = AbstractHaskell.Let (parseBinds binds) (parseExpr e)
parseExpr (If l e1 e2 e3)                         = IfThenElse (parseExpr e1) (parseExpr e2) (parseExpr e3)
parseExpr (Language.Haskell.Exts.Case l e alters) = AbstractHaskell.Case (parseExpr e) (map parseAlternatives alters)
parseExpr (Do  l stms)                            = DoExpr (map parseStms stms)
parseExpr (Language.Haskell.Exts.Tuple l Boxed es)= AbstractHaskell.Tuple (map parseExpr es)
parseExpr (Language.Haskell.Exts.List l exprs)    = AbstractHaskell.List (map parseExpr exprs)
parseExpr (Paren l e)                             = parseExpr e
parseExpr (Language.Haskell.Exts.ListComp l e qs) = AbstractHaskell.ListComp (parseExpr e) (map parseQualsStms qs)
parseExpr _   = undefined

rightHandtoExp = undefined

parsePatterns :: Pat l -> Pattern
parsePatterns (Language.Haskell.Exts.PVar _ name)         = AbstractHaskell.PVar (0,parsename name) -- Umgebung für Variablen
parsePatterns (Language.Haskell.Exts.PLit _ sign lit)     = AbstractHaskell.PLit (parseLiteral lit)
parsePatterns (PApp _ qn pats)                            = PComb ("",parseQName qn) (map parsePatterns pats) -- modulname
parsePatterns (Language.Haskell.Exts.PTuple _ Boxed pats) = AbstractHaskell.PTuple (map parsePatterns pats)
parsePatterns (Language.Haskell.Exts.PList _ pats)        = AbstractHaskell.PList (map parsePatterns pats)
parsePatterns (PParen l pat)                              = parsePatterns pat
parsePatterns (PAsPat _ name pat)                         = PAs (0,parsename name)(parsePatterns pat) -- Umgebung für Variablen
parsePatterns   _ = undefined

parseStms :: Stmt l -> Statement
parseStms (Generator _ pat e) = SPat  (parsePatterns pat) (parseExpr e)
parseStms (Qualifier _ e)     = SExpr (parseExpr e)
parseStms (LetStmt _ binds)   = SLet (parseBinds binds)
parseStms _                   = undefined

parseFuncPatDecls :: [Decl l] -> [LocalDecl]
parseFuncPatDecls []                      = []
parseFuncPatDecls ((s@(FunBind _ matches)):xs) = undefined--(LocalFunc parseFunDecl "modulname" --hier noch den typ s)
parseFuncPatDecls ((PatBind _ pat rhs wbind):xs) = undefined--(LocalPat (parsePatterns pat) parseRigthHands parseLocals wbinds)
parseFuncPatDecls _                         = []

parseBinds :: Binds l -> [LocalDecl]
parseBinds (BDecls l decls) = parseFuncPatDecls decls
parseBinds _ = []

parseAlternatives :: Alt l -> BranchExpr
parseAlternatives (Alt _ pat rhs _) = Branch (parsePatterns pat) (rightHandtoExp rhs)

parseQOp :: QOp l -> AbstractHaskell.QName -- Modulname noch hinzufügen
parseQOp (QVarOp _ qn) = ("",parseQName qn)
parseQOp (QConOp _ qn) = ("", parseQName qn)

parseQualsStms :: QualStmt l -> Statement
parseQualsStms (QualStmt _  stm )= parseStms stm
parseQualsStms _ = undefined

parseLiteral :: Language.Haskell.Exts.Literal l -> AbstractHaskell.Literal
parseLiteral (Char _ c _)     = Charc c
parseLiteral (String _ str _) = Stringc str
parseLiteral (Int _ i _)      = Intc $ fromInteger i
parseLiteral (Frac _ r _)     = Floatc $ fromRational r
parseLiteral _ = undefined

searchForType :: String -> [(Name l, Type l)] -> Type l
searchForType name ((n , t):nts) | name == (parsename n) = t
                                 | otherwise = searchForType name nts

buildType :: String -> Type l -> TypeSig
buildType modname x@(TyVar l name)          = CType [] (parseTyp modname x)
buildType modname x@(TyFun l t1 t2)         = CType [] (parseTyp modname x)
buildType modname x@(TyTuple l Boxed types) = CType [] (parseTyp modname x)
buildType modname x@(TyList l typ)          = CType [] (parseTyp modname x)
buildType modname x@(TyCon l qname)         = CType [] (parseTyp modname x)
buildType modname x@(TyParen l t)           = CType [] (parseTyp modname x)
buildType _       _                         = Untyped

parseTyp :: String -> Type l -> TypeExpr
parseTyp _    (TyVar l name)          = TVar (0, parsename name) -- hier noch eine Umgebung, die Ints generiert
parseTyp modu (TyFun l t1 t2)         = FuncType (parseTyp modu t1)(parseTyp modu t2)
parseTyp modu (TyTuple l Boxed types) = TCons (modu, "tupel") (map (parseTyp modu) types)
parseTyp modu (TyList l typ)          = TCons (modu, "list") [(parseTyp modu typ)]
parseTyp modu (TyCon l qname)         = TCons (modu, parseQName qname) []
parseTyp modu (TyParen l t)           = TCons (modu, "paren") [(parseTyp modu t)]

parseQName :: Language.Haskell.Exts.QName l -> String
parseQName (Qual l mdn name) = parsename name
parseQName (UnQual l name)   = parsename name
parseQName _                 = ""

parseArity :: [Match l] -> Int
parseArity []                                        = 0
parseArity ((InfixMatch _ _ _ _ _ _):ms)             = 2
parseArity ((Match l name patterns rhs wbinds) : ms) = length patterns

parseMatchName :: Match l -> String
parseMatchName (Match l name patterns rhs wbinds) = parsename name
parseMatchName (InfixMatch l pat1 name pat2 rhs wbinds) = parsename name

parsename :: Name l -> String
parsename (Ident l name)  = name
parsename (Language.Haskell.Exts.Symbol l name) = name

parseImportList :: ImportDecl l -> String
parseImportList x@(ImportDecl _ name _ _ _ _ _ _ ) = parseModuleName name

parseModuleHead :: Maybe (ModuleHead l) -> String
parseModuleHead Nothing                   = ""
parseModuleHead (Just (ModuleHead l n _ _)) = parseModuleName n

parseModuleName :: ModuleName l -> String
parseModuleName (ModuleName l str) = str

parseFile' :: FilePath -> IO (Module SrcSpanInfo)
parseFile' f = do
  (ParseOk ast) <- parseFile f
  return ast

parseTypeSignatur :: Module l -> [(Name l, Type l)]
parseTypeSignatur (Module l mh mp impdec decls) = parseDecls decls
parseTypeSignatur _ = []

parseDecls :: [Decl l] -> [(Name l,(Type l))]
parseDecls [] = []
parseDecls (x:xs) = parseOneDecl x ++ parseDecls xs

parseOneDecl :: Decl l -> [(Name l,(Type l ))]
parseOneDecl (TypeSig l names t) = concatMap (parseTypeSig t) names
parseOneDecl _ = []

parseTypeSig :: Type l -> Name l ->[(Name l,(Type l ))]
parseTypeSig typ name = [(name,typ)]
