import Language.Haskell.Exts
import Data.Functor
import AbstractHaskell

-- Umgebung die Ints generiert bauen

astToAbstractHaskell :: (Module l) -> Prog l
astToAbstractHaskell modu@(Module l modh mp imps declas) =
  Prog l mn (map (parseImportList) imps) (map (parseTypDecls mn) declas) (map (parseFunDecls mn typsignatures) declas) (concatMap (parseOpDecls mn) declas)
    where mn = parseModuleHead modh
          typsignatures = parseTypeSignatur modu
astToAbstractHaskell _ = Prog undefined "" [] [] [] []

-------------------------------------------------------------------------------------------------------------------------------------------------------
-- ERSTELLUNG VON OPDECLS -----------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------

parseOpDecls :: String -> Decl l -> [OpDecl l]
parseOpDecls str (InfixDecl l assoc (Just mInt) ops) = map (parseOps str (parseAssoc assoc) mInt) ops
parseOpDecls str (InfixDecl l assoc Nothing ops)     = map (parseOps str (parseAssoc assoc) (-1)) ops
parseOpDecls _ _                                     = undefined

parseAssoc :: Assoc l -> AbstractHaskell.Fixity l
parseAssoc (AssocNone l)  = InfixOp l
parseAssoc (AssocLeft l)  = InfixlOp l
parseAssoc (AssocRight l) = InfixrOp l

parseOps :: String -> AbstractHaskell.Fixity l -> Int -> Op l ->  OpDecl l
parseOps str fixity mInt (VarOp l name) = Op l (l,str,parsename name) fixity mInt
parseOps str fixity mInt (ConOp l name) = Op l (l,str,parsename name) fixity mInt

---------------------------------------------------------------------------------------------------------------------------------------------------------
-- ERSTELLUNG VON TYPEDECLS -----------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------
parseTypDecls :: String -> Decl l -> AbstractHaskell.TypeDecl l
parseTypDecls str (Language.Haskell.Exts.TypeDecl l declhead typ)     =
  TypeSyn l (parseTypeName str declhead) Public (parseTypeVariables typ) $  parseTyp str typ
parseTypDecls str (DataDecl l (DataType _) Nothing declhead qualcondecls mderv) =
  Type l (parseTypeName str declhead) Public (concatMap extractTypvariables qualcondecls) (map (parseQualConDecl str) qualcondecls)
parseTypDecls _ _                                                     = undefined

extractTypvariables :: QualConDecl l -> [TVarIName l]
extractTypvariables (QualConDecl _ (Just tvb) _ _) = map parseTVB tvb
extractTypvariables _                              = []

parseTVB :: TyVarBind l -> TVarIName l
parseTVB (KindedVar l name _) = (l,0,parsename name)
parseTVB (UnkindedVar l name) = (l,0,parsename name)

parseQualConDecl :: String -> QualConDecl l -> ConsDecl l
parseQualConDecl str (QualConDecl _ _ _ conDecl) = parseConDecl str conDecl

parseConDecl :: String -> ConDecl l -> ConsDecl l
parseConDecl str (ConDecl l name typs) = AbstractHaskell.Cons l (l,str,parsename name) (length typs) Public (map (parseTyp str) typs)
parseConDecl _  _                      = undefined

parseTypeVariables :: Type l -> [TVarIName l]
parseTypeVariables (TyVar l name)      = [(l,0, parsename name)] -- hier noch umgebung für variablen
parseTypeVariables (TyFun _ t1 t2)     = parseTypeVariables t1 ++ parseTypeVariables t2
parseTypeVariables (TyTuple _ _ x)     = concatMap parseTypeVariables x
parseTypeVariables (TyList _ t)        = parseTypeVariables t
parseTypeVariables (TyApp _ t1 t2)     = parseTypeVariables t1 ++ parseTypeVariables t2
parseTypeVariables (TyParen _ t)       = parseTypeVariables t
parseTypeVariables (TyInfix _ t1 _ t2) = parseTypeVariables t1 ++ parseTypeVariables t2
parseTypeVariables _                   = []

parseTypeName :: String -> DeclHead l -> AbstractHaskell.QName l
parseTypeName mn (DHead l name) = (l,mn,parsename name)
parseTypeName mn (DHInfix l _ name)  = (l,mn,parsename name)
parseTypeName mn (DHParen l dh)     = parseTypeName mn dh
parseTypeName mn (DHApp l dh _)     = parseTypeName mn dh

---------------------------------------------------------------------------------------------------------------------------------------------------------
-- ERSTELLUNG VON FUNCDECLS -----------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------

-- Hier noch für pattern definitionen
parseFunDecls :: String ->  [(Name l, Type l)] -> Decl l -> FuncDecl l
parseFunDecls modulename typsignatures (FunBind l mas@(m:matches)) =
  Func  l "" (l,modulename, functionname) (parseArity mas) Public (buildType modulename (searchForType functionname typsignatures)) (Rules l (map (parseRules modulename typsignatures) mas))
    where functionname = parseMatchName m
--parseFunDecls modulename typsignatures (PatBind l pat rhs mbinds) =
--  Func l "" (l,modulename,functionname) 0 Public Untyped (Rules undefined [])
  --  where functionname = parseMatchName mas
parseFunDecls modulename typsignatures _ = Func undefined "" (undefined,modulename,"") 0 Public Untyped (Rules undefined [])


parseRules :: String -> [(Name l, Type l)]-> Match l -> AbstractHaskell.Rule l
parseRules str t (Match l _ pats rhs _)         = parseRule str t pats rhs
parseRules _ t (InfixMatch _ pat1 _ pats rhs _) = undefined

parseRule :: String ->[(Name l, Type l)]-> [Pat l] -> Language.Haskell.Exts.Rhs l -> AbstractHaskell.Rule l
parseRule str t pats (UnGuardedRhs l expr)  = AbstractHaskell.Rule l (map (parsePatterns str) pats) (SimpleRhs l (parseExpr str t expr)) (parseLocal str t  expr)
parseRule str t pats (GuardedRhss l gurhss) = AbstractHaskell.Rule l (map (parsePatterns str) pats) (AbstractHaskell.GuardedRhs l (concatMap (parseGuarded str t) gurhss)) []

parseFuncPatDecls :: String -> [(Name l, Type l)] -> [Decl l] -> [LocalDecl l]
parseFuncPatDecls _ _ []                               = []
parseFuncPatDecls str t ((s@(FunBind l matches)):xs)   = [LocalFunc l (parseFunDecls str t s)]
parseFuncPatDecls str t ((PatBind l pat rhs (Just wbind)):xs) = [LocalPat l (parsePatterns str pat) (parseRigthHands str t rhs) (parseBinds str t wbind)]
parseFuncPatDecls _  _  _                              = []

parseRigthHands :: String -> [(Name l, Type l)]-> Language.Haskell.Exts.Rhs l -> AbstractHaskell.Rhs l
parseRigthHands str t (UnGuardedRhs l e) = SimpleRhs l (parseExpr str t e)
parseRigthHands str t (GuardedRhss l grhs) = AbstractHaskell.GuardedRhs l ( concatMap (parseGuarded str t) grhs)

parseLocal :: String -> [(Name l, Type l)] -> Exp l -> [LocalDecl l]
parseLocal str t(Language.Haskell.Exts.Let l binds e) = parseBinds str t binds
parseLocal _ _ _                = []

parseGuarded :: String ->[(Name l, Type l)]->  GuardedRhs l ->[(Statement l, Expr l)]
parseGuarded str t (Language.Haskell.Exts.GuardedRhs l stmts expr ) =  map (splitStaments str t expr) stmts

splitStaments :: String -> [(Name l, Type l)] -> Exp l -> Stmt l -> (Statement l, Expr l)
splitStaments str t e stmt = (parseStms str t stmt , parseExpr str t e)

parseExpr :: String -> [(Name l, Type l)] -> Exp l -> Expr l
parseExpr _ t (Language.Haskell.Exts.Var l qn)        = AbstractHaskell.Var l (l,0,parseQName qn)-- varialben umgebung hinzufügen
parseExpr mn t (Con l qn)                              = AbstractHaskell.Symbol l (l,mn,parseQName qn)
parseExpr _ t (Language.Haskell.Exts.Lit l lit)       = AbstractHaskell.Lit l (parseLiteral lit)
parseExpr mn t (InfixApp l exp1 qop exp2)              = InfixApply l (parseExpr mn t exp1) (parseQOp mn qop) (parseExpr mn t exp2)
parseExpr mn t (Language.Haskell.Exts.Lambda l pats e) = AbstractHaskell.Lambda l (map (parsePatterns mn) pats) (parseExpr mn t e)
parseExpr mn t (Language.Haskell.Exts.Let l binds e)   = AbstractHaskell.Let l (parseBinds mn t binds) (parseExpr mn t e)
parseExpr mn t (If l e1 e2 e3)                         = IfThenElse l (parseExpr mn t e1) (parseExpr mn t e2) (parseExpr mn t e3)
parseExpr mn t (Language.Haskell.Exts.Case l e alters) = AbstractHaskell.Case l (parseExpr mn t e) (map (parseAlternatives mn t) alters)
parseExpr mn t (Do  l stms)                            = DoExpr l (map (parseStms mn t) stms)
parseExpr mn t (Language.Haskell.Exts.Tuple l Boxed es)= AbstractHaskell.Tuple l (map (parseExpr mn t) es)
parseExpr mn t (Language.Haskell.Exts.List l exprs)    = AbstractHaskell.List l (map (parseExpr mn t) exprs)
parseExpr mn t (Paren _ e)                            = parseExpr mn t e
parseExpr mn t (Language.Haskell.Exts.ListComp l e qs) = AbstractHaskell.ListComp l (parseExpr mn t e) (map (parseQualsStms  mn t) qs)
parseExpr _  _ _ = undefined

rightHandtoExp :: String -> [(Name l, Type l)] -> Language.Haskell.Exts.Rhs l -> Expr l
rightHandtoExp str t (UnGuardedRhs _ e) = parseExpr str  t e
rightHandtoExp str t (GuardedRhss _ gdrhs) = undefined

parsePatterns :: String -> Pat l -> Pattern l
parsePatterns mn (Language.Haskell.Exts.PVar l name)         = AbstractHaskell.PVar l (l,0,parsename name) -- Umgebung für Variablen
parsePatterns mn (Language.Haskell.Exts.PLit l sign lit)     = AbstractHaskell.PLit l (parseLiteral lit)
parsePatterns mn (PApp l qn pats)                            = PComb l (l,mn,parseQName qn) (map (parsePatterns mn) pats)
parsePatterns mn (Language.Haskell.Exts.PTuple l Boxed pats) = AbstractHaskell.PTuple l (map (parsePatterns mn) pats)
parsePatterns mn (Language.Haskell.Exts.PList l pats)        = AbstractHaskell.PList l (map (parsePatterns mn) pats)
parsePatterns mn (PParen _ pat)                              = parsePatterns mn pat
parsePatterns mn (PAsPat l name pat)                         = PAs l (l,0,parsename name)(parsePatterns mn pat) -- Umgebung für Variablen
parsePatterns _  _                                           = undefined

parseStms :: String -> [(Name l, Type l)] -> Stmt l -> Statement l
parseStms str t (Generator l pat e) = SPat l (parsePatterns str pat) (parseExpr str t e)
parseStms str t (Qualifier l e)     = SExpr l (parseExpr str t e)
parseStms str t (LetStmt l binds)     = SLet l (parseBinds str t binds)
parseStms _  _ _                   = undefined

parseBinds :: String -> [(Name l, Type l)] -> Binds l -> [LocalDecl l]
parseBinds str t (BDecls _ decls) = parseFuncPatDecls str t decls
parseBinds _ _ _  = []

parseAlternatives ::String -> [(Name l, Type l)] -> Alt l -> BranchExpr l
parseAlternatives str t (Alt l pat rhs _) = Branch l (parsePatterns str pat) (rightHandtoExp str t rhs)

parseQOp :: String -> QOp l -> AbstractHaskell.QName l
parseQOp mn (QVarOp l qn) = (l,mn,parseQName qn)
parseQOp mn (QConOp l qn) = (l,mn, parseQName qn)

parseQualsStms :: String -> [(Name l, Type l)] ->  QualStmt l -> Statement l
parseQualsStms str t (QualStmt _ stm )= parseStms str t stm
parseQualsStms _ _ _ = undefined

parseLiteral :: Language.Haskell.Exts.Literal l -> AbstractHaskell.Literal
parseLiteral (Char _ c _)     = Charc c
parseLiteral (String _ str _) = Stringc str
parseLiteral (Int _ i _)      = Intc $ fromInteger i
parseLiteral (Frac _ r _)     = Floatc $ fromRational r
parseLiteral _ = undefined

parseTyp :: String -> Type l -> TypeExpr l
parseTyp _    (TyVar l name)          = TVar l (l,0, parsename name) -- hier noch eine Umgebung, die Ints generiert
parseTyp modu (TyFun l t1 t2)         = FuncType l (parseTyp modu t1)(parseTyp modu t2)
parseTyp modu (TyTuple l Boxed types) = TCons l (l,modu, "tupel") (map (parseTyp modu) types)
parseTyp modu (TyList l typ)          = TCons l (l,modu, "list") [(parseTyp modu typ)]
parseTyp modu (TyCon l qname)         = TCons l (l,modu, parseQName qname) []
parseTyp modu (TyParen l t)           = TCons l (l,modu, "paren") [(parseTyp modu t)]

parseArity :: [Match l] -> Int
parseArity []                                        = 0
parseArity ((InfixMatch _ _ _ _ _ _):ms)             = 2
parseArity ((Match l name patterns rhs wbinds) : ms) = length patterns

parseImportList :: ImportDecl l -> String
parseImportList x@(ImportDecl _ name _ _ _ _ _ _ ) = parseModuleName name

parseModuleHead :: Maybe (ModuleHead l) -> String
parseModuleHead Nothing                   = ""
parseModuleHead (Just (ModuleHead l n _ _)) = parseModuleName n

parseModuleName :: ModuleName l -> String
parseModuleName (ModuleName l str) = str

parseDecls :: [Decl l] -> [(Name l,(Type l))]
parseDecls [] = []
parseDecls (x:xs) = parseOneDecl x ++ parseDecls xs

parseOneDecl :: Decl l -> [(Name l,(Type l ))]
parseOneDecl (TypeSig l names t) = concatMap (parseTypeSig t) names
parseOneDecl _ = []

---------------------------------------------------------------------------------------------------------------------------------------------------------
-- ALLGEMEINE FUNKTIONEN --------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------

parseTypeSig :: Type l -> Name l ->[(Name l,(Type l ))]
parseTypeSig typ name = [(name,typ)]

parseTypeSignatur :: Module l -> [(Name l, Type l)]
parseTypeSignatur (Module l mh mp impdec decls) = parseDecls decls
parseTypeSignatur _ = []

parseFile' :: FilePath -> IO (Module SrcSpanInfo)
parseFile' f = do
        (ParseOk ast) <- parseFile f
        return ast

parseMatchName :: Match l -> String
parseMatchName (Match l name patterns rhs wbinds) = parsename name
parseMatchName (InfixMatch l pat1 name pat2 rhs wbinds) = parsename name

parsename :: Name l -> String
parsename (Ident l name)  = name
parsename (Language.Haskell.Exts.Symbol l name) = name

parseQName :: Language.Haskell.Exts.QName l -> String
parseQName (Qual l mdn name) = parsename name
parseQName (UnQual l name)   = parsename name
parseQName _                 = ""

searchForType :: String -> [(Name l, Type l)] -> Type l
searchForType name ((n , t):nts) | name == (parsename n) = t
                                 | otherwise = searchForType name nts

buildType :: String -> Type l -> TypeSig l
buildType modname x@(TyVar l name)          = CType l [] (parseTyp modname x)
buildType modname x@(TyFun l t1 t2)         = CType l  [] (parseTyp modname x)
buildType modname x@(TyTuple l Boxed types) = CType l [] (parseTyp modname x)
buildType modname x@(TyList l typ)          = CType l [] (parseTyp modname x)
buildType modname x@(TyCon l qname)         = CType l [] (parseTyp modname x)
buildType modname x@(TyParen l t)           = CType l [] (parseTyp modname x)
buildType _       _                         = Untyped

