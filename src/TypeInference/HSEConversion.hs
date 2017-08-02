{-# LANGUAGE FlexibleContexts      #-}

module TypeInference.HSEConversion
  ( hseToNLAH, parseNamePattern, findDifference
  ) where

import           Control.Monad.State.Lazy
import           Data.Map.Lazy                 as DML
import           Language.Haskell.Exts         as HSE
import           TypeInference.AbstractHaskell as AH
import           TypeInference.TypeSig
import           TypeInference.AbstractHaskellGoodies

-------------------------------------------------------------------------------
-- STATE FOR VARIABLEINDEX ----------------------------------------------------
-------------------------------------------------------------------------------

-- | State for variableindex
--   idx is the next free index
--   vmap contains all already seen variables and their index
data AHState = AHState { idx      :: Int
                       , vmap     :: Map String Int
                       , fctNames :: [String]
                       }

-- | Initionalstate for AHState
initialState = AHState 0 empty []

-- | Returns the index of a variable
--   for an unseen variable the next possible state is returned
--   for a seen variable the given index is returned
getidx :: MonadState AHState m => String -> m Int
getidx name = do
    ahs <- get
    case DML.lookup name (vmap ahs) of
      Just x -> return x
      Nothing -> do
                   let idx' = idx ahs
                   put AHState {idx= idx' +1 , vmap=insert name idx' $ vmap ahs,fctNames = fctNames ahs}
                   return idx'

-------------------------------------------------------------------------------
-- TRANSFORMATION HSE TO AH  --------------------------------------------------
-------------------------------------------------------------------------------

-- | Transforms a haskell-src-extensions module into an abstract haskell
--   program
hseToNLAH :: DML.Map AH.QName (TypeExpr a) -> Module a -> Prog a
hseToNLAH mapTE modu = evalState (astToAbstractHaskell mapTE modu) initialState

--astToAbstractHaskell :: MonadState AHState m => Module a -> m (Prog a)
astToAbstractHaskell mapTE modu@(Module l modh mp imps declas) =
  do
    getFunctionNames modu
    st <- get
    let defFcts = fctNames st
    let mapAsList = toList mapTE
    let allKeys =  collectKeys mapAsList
    let allKeyFctNames = extractOutOfQName allKeys
    let difBtDefAndPre = findDifference defFcts allKeyFctNames
    let addedPrefix = addPre difBtDefAndPre allKeyFctNames
    put AHState {idx = idx st, vmap = vmap st, fctNames = fctNames st ++ addedPrefix}
    let mn = parseModuleHead modh
    let ts = parseTypeSignatur modu
    let il = parseImportList imps
    tdcl <- mapM (parseTypDecls mn) $ filterdecls declas
    fdcl <- mapM (parseFunDecls mn ts) $ filterFunDecls declas
    return $ Prog (mn,l) il tdcl fdcl
astToAbstractHaskell _ _ = return $ Prog ("",undefined) [] [] []

collectKeys :: [(a,b)] -> [a]
collectKeys []         = []
collectKeys ((a,b):xs) = [a] ++ collectKeys xs

-- | Returns the difference of two lists
findDifference :: Eq a => [a] -> [a] -> [a]
findDifference xs ys = [y | y <- ys, y `notElem` xs]

extractOutOfQName :: [(MName,String)] -> [String]
extractOutOfQName []         = []
extractOutOfQName ((a,b):xs) = [b] ++ collectKeys xs

addPre :: [String] -> [String] -> [String]
addPre [] _         = []
addPre (x:xs) mapkv = case elem x mapkv of
                        False -> x : addPre xs mapkv
                        True  -> ("Prelude"++ x): (addPre xs mapkv)
-------------------------------------------------------------------------------
-- CREATION OF TYPEDECLS ------------------------------------------------------
-------------------------------------------------------------------------------

-- | Transforms a Typedeclaration
parseTypDecls :: MonadState AHState m => MName -> Decl l -> m (TypeDecl l)
parseTypDecls str (HSE.TypeDecl l declhead typ)               =
   do
     t <- parseTyp str typ
     tv <- parseTypeVariables typ
     return $ TypeSyn l (parseTypeName str declhead,l) Public tv t
parseTypDecls str (DataDecl l (DataType _) Nothing declhead qualcondecls _) =
  do
    etv <- mapM  extractTypvariables $ filterqual qualcondecls
    let he = concat etv
    qcd <- mapM (parseQualConDecl str) qualcondecls
    return $ Type l (parseTypeName str declhead,l) Public he qcd

-- | Extracts the typevariables out of a qualified constructor declaration
extractTypvariables ::
  MonadState AHState m => QualConDecl l -> m [(VarName, l)]
extractTypvariables (QualConDecl _ (Just tvb) _ _) =
  mapM parseTVB tvb

-- | Parses a typevariable
parseTVB :: MonadState AHState m => TyVarBind l -> m (VarName, l)
parseTVB (KindedVar l name _) = do
                                  y <- getidx (parsename name)
                                  return ((y, parsename name),l)
parseTVB (UnkindedVar l name) = do
                                  y <- getidx (parsename name)
                                  return ((y, parsename name),l)

-- | Parses a qualified constructer name
parseQualConDecl ::
  MonadState AHState m => MName -> QualConDecl l -> m (ConsDecl l)
parseQualConDecl str (QualConDecl _ _ _ conDecl) =
  parseConDecl str conDecl

-- | Parses a constructer declaration
parseConDecl :: MonadState AHState m => MName -> ConDecl l -> m (ConsDecl l)
parseConDecl str (ConDecl l name typs)       =
  do
    tps <- mapM (parseTyp str) typs
    return $ AH.Cons l ((str,parsename name),l) (length typs) Public tps
parseConDecl str (InfixConDecl l t1 name t2) =
  do
    tp1 <- parseTyp str t1
    tp2 <- parseTyp str t2
    let tp = tp1 : [tp2]
    return $ AH.Cons l ((str,parsename name),l) (length tp) Public tp
parseConDecl _  _                            =
  error "parseConDecl"

-- | Parses a typevariables out of a type
parseTypeVariables :: MonadState AHState m => Type l -> m [(VarName, l)]
parseTypeVariables (TyVar l name)      =
  do
                                          y <- getidx (parsename name)
                                          return [((y, parsename name),l)]
parseTypeVariables (TyFun _ t1 t2)     =
  do
                                          pv1 <- parseTypeVariables t1
                                          pv2 <- parseTypeVariables t2
                                          return $ pv1 ++ pv2
parseTypeVariables (TyTuple _ _ x)     =
  do
                                           pl <- mapM parseTypeVariables x
                                           let pv = concat pl
                                           return pv
parseTypeVariables (TyList _ t)        =
  parseTypeVariables t
parseTypeVariables (TyApp _ t1 t2)     =
  do
    pv1 <- parseTypeVariables t1
    pv2 <- parseTypeVariables t2
    return $ pv1 ++ pv2
parseTypeVariables (TyParen _ t)       =
  parseTypeVariables t
parseTypeVariables (TyInfix _ t1 _ t2) =
  do
    pv1 <- parseTypeVariables t1
    pv2 <- parseTypeVariables t2
    return $ pv1 ++ pv2
parseTypeVariables _                   =
  return []

-- | Parses a datatype name
parseTypeName :: String -> DeclHead l -> AH.QName
parseTypeName mn (DHead l name)     = (mn,parsename name)
parseTypeName mn (DHInfix l _ name) = (mn,parsename name)
parseTypeName mn (DHParen l dh)     = parseTypeName mn dh
parseTypeName mn (DHApp l dh _)     = parseTypeName mn dh

-------------------------------------------------------------------------------
-- CREATION OF FUNCDECLS ------------------------------------------------------
-------------------------------------------------------------------------------

type TypeS l = [(Name l,  Type l)]

-- | Parses a function declaration
parseFunDecls ::
  MonadState AHState m => MName -> TypeS l -> Decl l -> m (FuncDecl l)
parseFunDecls modu ts (FunBind l mas@(m:matches)) =
   do
     let fn = parseMatchName m
     ahs <- get
     put $ AHState {idx = 0 , vmap = empty, fctNames = fctNames ahs }
     case (searchForType fn ts) of
       Nothing -> do
                    put $ AHState {idx = 0, vmap = empty, fctNames = fctNames ahs }
                    rl <- mapM (parseRules modu ts) mas
                    let r = Rules rl
                    let fname = ((modu,fn),l)
                    let ar = parseArity mas
                    return $ Func l fname ar Public Untyped r
       Just z -> do
                   btd <- buildType l modu z
                   put $ AHState {idx = 0, vmap = empty, fctNames = fctNames ahs}
                   rl <- mapM (parseRules modu ts) mas
                   let r = Rules rl
                   let fname = ((modu,fn),l)
                   let ar = parseArity mas
                   return $ Func l fname ar Public btd r
parseFunDecls modu ts (PatBind l pat rhs mbinds)  =
  do
    let pn = parseNameOutOfPattern pat
    rl <- parseRulesOutOfPats modu ts pat rhs
    case (searchForType pn ts) of
      Nothing -> return $ Func l ((modu,pn),l) 0 Public Untyped rl
      Just z -> do
                  btd <- buildType l modu z
                  return $ Func l ((modu,pn),l) 0 Public btd rl
parseFunDecls modu ts  _                          =
  do
    let fname = ((modu,""), undefined)
    return $ Func undefined fname 0 Public Untyped (Rules [])

-- | parses rules out of patterns
parseRulesOutOfPats ::
  MonadState AHState m => MName -> TypeS a -> Pat a -> HSE.Rhs a -> m (Rules a)
parseRulesOutOfPats  modu ts pat rhs  =
  do
    rs <- parseRule modu ts [pat] rhs
    return $ Rules [rs]

-- | Parses rules
parseRules ::
  MonadState AHState m => MName -> TypeS a -> Match a -> m (AH.Rule a)
parseRules str t (Match l _ pats rhs wbinds)             =
  do
    r@(AH.Rule l tya p rh loc) <- parseRule str t pats rhs
    case wbinds of
      Nothing -> return $ AH.Rule l tya p rh (loc)
      Just y -> do
                  locs <- (parseBinds str t) y
                  return $ AH.Rule l tya p rh (loc ++ locs)
parseRules str t (InfixMatch l pat name pats rhs mbinds) =
  do
    r@(AH.Rule l tya p rh loc) <- parseRule str t ([pat] ++ pats) rhs
    case mbinds of
      Nothing -> return $ AH.Rule l tya p rh (loc)
      Just y -> do
                  locs <- (parseBinds str t) y
                  return $ AH.Rule l tya p rh (loc ++ locs)

-- | Parses a rule
parseRule ::
  MonadState AHState m => MName   ->
                          TypeS a ->
                          [Pat a] ->
                          HSE.Rhs a ->
                           m (AH.Rule a)
parseRule str t pats (UnGuardedRhs l expr)  =
  do
    patt <- mapM (parsePatterns str) pats
    ep <- parseExpr str t expr
    return $ AH.Rule l NoTypeAnn patt (SimpleRhs ep) []
parseRule str t pats (GuardedRhss l gurhss) =
  do
    patt <- mapM (parsePatterns str) pats
    g <- mapM (parseGuarded str t) gurhss
    return $ AH.Rule l NoTypeAnn patt (AH.GuardedRhs l (concat g)) []

-- | Parses a guarded right hand side
parseGuarded ::
  MonadState AHState m => MName ->
                          TypeS l ->
                          GuardedRhs l ->
                          m [(Expr l, Expr l)]
parseGuarded str t (HSE.GuardedRhs l stmts expr) =
  mapM (splitStatments str t expr) stmts

-- | Splits a statements into (expr1,expr2) with expr1 is the parsed statement
splitStatments ::
  MonadState AHState m => MName ->
                          TypeS l ->
                          Exp l ->
                          Stmt l ->
                          m (Expr l, Expr l)
splitStatments str t expr stmts =
  do
    expr2 <- parseExpr str t expr
    expr1 <- parseStmtsToExpr str t $ head  $ filterStmts [stmts]
    return (expr1,expr2)

-- | Parses a statement into an expr
parseStmtsToExpr ::
  MonadState AHState m => MName -> TypeS a -> Stmt a -> m (Expr a)
parseStmtsToExpr str t (Qualifier l expr) =
  parseExpr str t expr


-- | Parses functions and pattern bindings
parseFuncPatDecls ::
  MonadState AHState m => String -> TypeS a -> [Decl a] -> m [LocalDecl a]
parseFuncPatDecls _ _ []                                      =
  return []
parseFuncPatDecls str t ((FunBind l matches@(m:ms)):xs)       =
  do
    let fn = parseMatchName  m
    case (searchForType fn t) of
      Nothing -> do
                   rl <- mapM (parseRules str t) matches
                   let r = Rules rl
                   let fname = ((str,fn),l)
                   let ar = parseArity matches
                   return $ [LocalFunc $ Func l fname ar Public Untyped r]
      Just z -> do
                  btd <- buildType l str z
                  rl <- mapM (parseRules str t) matches
                  let r = Rules rl
                  let fname = ((str,fn),l)
                  let ar = parseArity matches
                  return $ [LocalFunc $ Func l fname ar Public btd r]
parseFuncPatDecls str t
  (PatBind l pat rhs@(UnGuardedRhs a expr) (Just wbind) : xs) =
   do
    rh <- parseExprOutOfRhs str t rhs
    patt <- parsePatterns str pat
    bnd <- parseBinds str t wbind
    return [LocalPat l patt rh bnd]
parseFuncPatDecls str t
  ((PatBind l pat rhs@(GuardedRhss a gds) (Just b)):xs) |length gds <= 1 =
   do
     rh <- parseExprOutOfGrd str t (head gds)
     patt <- parsePatterns str pat
     bnd <- parseBinds str t b
     return $ [LocalPat l patt rh bnd]
                                                        |otherwise      =
   do
     let name = parseNameOutOfPattern pat
     rh <- parseExprOutOfGrd str t (head gds)
     patt <- parsePatterns str pat
     bnd <- parseBinds str t b
     rules <- mapM (parseRulesOutOfGuarded str t) gds
     let rulesR = makeRules rules
     let n = newRule l patt rulesR bnd
     return $ [LocalFunc $ Func l (("",name),l) 0 Public Untyped (Rules [n])]
parseFuncPatDecls _  _  _                                     =
  return []

-- | Builds a new Rule
newRule :: a -> Pattern a -> AH.Rhs a -> [LocalDecl a] -> AH.Rule a
newRule l pat rhs b = AH.Rule l NoTypeAnn [pat] rhs b

-- | Makes one right hand side out of a list of right hand sides
makeRules :: [AH.Rhs a] -> AH.Rhs a
makeRules xs = let r =  makeRules' xs
                  in SimpleRhs $ AH.List undefined NoTypeAnn r

-- | transforms tupel into a list
toExprList :: [(t, t)] -> [t]
toExprList  []        =[]
toExprList ((a,b):xs) = a: b: toExprList xs

makeRules' :: [AH.Rhs t] -> [Expr t]
makeRules' ((AH.GuardedRhs a es):xs) = (toExprList es) ++ makeRules' xs

parseRulesOutOfGuarded
  :: MonadState AHState m => MName -> TypeS a -> GuardedRhs a -> m (AH.Rhs a)
parseRulesOutOfGuarded str t (HSE.GuardedRhs l stmts expr) = do
  tups <- mapM (parseTupels str t expr) stmts
  return $ AH.GuardedRhs l tups

-- | Parses a tupel
parseTupels ::
 MonadState AHState m => MName ->
                         TypeS a ->
                         Exp a ->
                         Stmt a ->
                         m (Expr a, Expr a)
parseTupels str t expr2 (Qualifier l expr1) =
  do
    ex1 <- parseExpr str t expr1
    ex2 <- parseExpr str t expr2
    return (ex1,ex2)

-- | Parses an expr out of a unguarded right hand side
parseExprOutOfRhs ::
  MonadState AHState m => MName -> TypeS a -> HSE.Rhs a -> m (Expr a)
parseExprOutOfRhs str t (UnGuardedRhs l expr) =
  parseExpr str t expr

-- | Parses an expr out of a guarded right hand side
parseExprOutOfGrd ::
  MonadState AHState m => MName -> TypeS a -> GuardedRhs a -> m (Expr a)
parseExprOutOfGrd str t (HSE.GuardedRhs l stmts expr) =
  parseExpr str t expr

-- | parses a right hand side
parseRigthHands ::
  MonadState AHState m => MName -> TypeS a -> HSE.Rhs a -> m (AH.Rhs a)
parseRigthHands str t (UnGuardedRhs l e)   =
  do
    expr <- parseExpr str t e
    return $ SimpleRhs expr
parseRigthHands str t (GuardedRhss l grhs) =
  do
    gud <- mapM (parseGuarded str t) grhs
    return $ AH.GuardedRhs l (concat gud)

-- | parses a Local declaration out of an expr
parseLocal ::
  MonadState AHState m => String -> TypeS a -> Exp a -> m [LocalDecl a]
parseLocal str t (HSE.Let l binds e) = parseBinds str t binds
parseLocal _ _ _                     = return []

-- | parses an expr
parseExpr :: MonadState AHState m => MName -> TypeS a -> Exp a -> m (Expr a)
parseExpr mn  _ (HSE.Var l qn)                    =
  do
    ahs <- get
    let name = parseQName qn
    case elem name (fctNames ahs) of
      True ->  return $ AH.Symbol NoTypeAnn ((mn, parseQName qn), l)
      False -> do
                 y <- getidx (parseQName qn)
                 return $ AH.Var NoTypeAnn ((y,parseQName qn),l)
    case elem ('P':'r':'e':'l':'u':'d':'e':name) (fctNames ahs) of
      True -> return $ AH.Symbol NoTypeAnn (("Prelude",parseQName qn),l)
      False -> do
                 y <- getidx (parseQName qn)
                 return $ AH.Var NoTypeAnn ((y,parseQName qn),l)
parseExpr mn _ (Con l qn)                        =
  return $ parseQNameForSpecial mn l qn
parseExpr _  _ (HSE.Lit l lit)                   =
  return $ AH.Lit NoTypeAnn (parseLiteral lit, l)
parseExpr mn t (InfixApp l exp1 qop exp2)        =
  do
    expr1 <- parseExpr mn t exp1
    expr2 <- parseExpr mn t exp2
    return $ InfixApply l NoTypeAnn expr1 (parseQOp mn qop, l) expr2
parseExpr mn t (HSE.Lambda l pats e)             =
  do
    expr <- parseExpr mn t e
    pat <- mapM (parsePatterns mn) pats
    return $ AH.Lambda l NoTypeAnn pat expr
parseExpr mn t (HSE.Let l binds e)               =
  do
    expr <- parseExpr mn t e
    bnd <- parseBinds mn t binds
    return $ AH.Let l NoTypeAnn bnd expr
parseExpr mn t (If l e1 e2 e3)                   =
  do
    expr1 <- parseExpr mn t e1
    expr2 <- parseExpr mn t e2
    expr3 <- parseExpr mn t e3
    return $ IfThenElse l NoTypeAnn expr1 expr2 expr3
parseExpr mn t (HSE.Case l e alters)             =
  do
    expr <- parseExpr mn t e
    alt <- mapM (parseAlternatives mn t) alters
    return $ AH.Case l NoTypeAnn expr alt
parseExpr mn t (Do  l stms)                      =
  do
    st <- mapM (parseStms mn t) stms
    return $ DoExpr l NoTypeAnn st
parseExpr mn t (HSE.Tuple l Boxed es)            =
  do
    e <- mapM (parseExpr mn t) es
    return $ AH.Tuple l NoTypeAnn e
parseExpr mn t (HSE.List l exprs)                =
  do
    eps <- mapM (parseExpr mn t) exprs
    return $ AH.List l NoTypeAnn eps
parseExpr mn t (Paren _ e)                       =
  parseExpr mn t e
parseExpr mn t (App l e1 e2)                     =
  do
    expr1 <- parseExpr mn t e1
    expr2 <- parseExpr mn t e2
    return $ Apply l NoTypeAnn expr1 expr2
parseExpr mn t (HSE.ListComp l e qs)             =
  do
    expr <- parseExpr mn t e
    let r = filterQualsStmts qs
    q <- mapM (parseQualsStms mn t) r
    return $ AH.ListComp l NoTypeAnn expr q
parseExpr mn t (ExpTypeSig l expr typ)           =
  do
    e <- parseExpr mn t expr
    t <- parseTyp mn typ
    return $ Typed l NoTypeAnn e t
parseExpr mn t (EnumFrom l expr)                 =
  do
    e <- parseExpr mn t expr
    return $ Apply l NoTypeAnn (AH.Lit NoTypeAnn (Stringc "enumFrom", l)) e
parseExpr mn t (EnumFromTo l exp1 exp2)          =
  do
    expr1 <- parseExpr mn t exp1
    expr2 <- parseExpr mn t exp2
    return $ Apply l NoTypeAnn (AH.Lit NoTypeAnn (Stringc "enumFromTo",l))
                               (Apply l NoTypeAnn expr1 expr2)
parseExpr mn t (EnumFromThen l exp1 exp2)        =
  do
    expr1 <- parseExpr mn t exp1
    expr2 <- parseExpr mn t exp2
    return $ Apply l NoTypeAnn (AH.Lit NoTypeAnn (Stringc "enumFromThen",l))
                              (Apply l NoTypeAnn expr1 expr2)
parseExpr mn t (EnumFromThenTo l exp1 exp2 exp3) =
  do
    expr1 <- parseExpr mn t exp1
    expr2 <- parseExpr mn t exp2
    expr3 <- parseExpr mn t exp3
    return $ Apply l NoTypeAnn (AH.Lit NoTypeAnn (Stringc "enumFromThenTo",l))
                   (Apply l NoTypeAnn  expr1 (Apply l NoTypeAnn expr2 expr3))
parseExpr _  _ _                                 =
  error "parseExpr"

parseQNameForSpecial mn l x@(Special a (UnitCon b))          =
    AH.Symbol NoTypeAnn ((mn,parseQName x), l)
parseQNameForSpecial mn l (Special a (ListCon b))          =
  AH.List b NoTypeAnn []
parseQNameForSpecial mn l x@(Special a (FunCon b))         =
  AH.Symbol NoTypeAnn ((mn,parseQName x), l)
parseQNameForSpecial mn l (Special a (TupleCon b box i))   =
  AH.Tuple a NoTypeAnn []
parseQNameForSpecial mn l (Special a(HSE.Cons b))          =
    AH.List b NoTypeAnn []
parseQNameForSpecial mn l (Special a (UnboxedSingleCon b)) =
  AH.Tuple a NoTypeAnn []
parseQNameForSpecial mn l x@(UnQual a name)                =
  AH.Symbol NoTypeAnn ((mn,parseQName x), l)
parseQNameForSpecial mn l x@(Qual a mon name)              =
  AH.Symbol NoTypeAnn ((mn,parseQName x), l)

-- | Transforms a right hand side to an expr
rightHandtoExp ::
  MonadState AHState m => MName -> TypeS a -> HSE.Rhs a -> m (Expr a)
rightHandtoExp str t (UnGuardedRhs _ e)    = parseExpr str t e
rightHandtoExp str t (GuardedRhss _ gdrhs) = error "rightHandtoExp"

-- | Parses a pattern
parsePatterns:: MonadState AHState m => MName -> Pat l -> m (Pattern l)
parsePatterns mn (HSE.PVar l name)         =
  do
    y <- getidx (parsename name)
    return $ AH.PVar NoTypeAnn ((y,parsename name),l)
parsePatterns mn (HSE.PLit l sign lit)     =
  return $ AH.PLit NoTypeAnn (parseLiteral lit, l)
parsePatterns mn (PApp l qn pats)          =
  do
    pat <- mapM (parsePatterns mn) pats
    return $ PComb l NoTypeAnn ((mn,parseQName qn),l) pat
parsePatterns mn (HSE.PTuple l Boxed pats) =
  do
    pat <- mapM (parsePatterns mn) pats
    return $ AH.PTuple l NoTypeAnn pat
parsePatterns mn (HSE.PList l pats)        =
  do
    pat <- mapM (parsePatterns mn) pats
    return $ AH.PList l NoTypeAnn pat
parsePatterns mn (PParen _ pat)            =
  parsePatterns mn pat
parsePatterns mn (PAsPat l name pat)       =
  do
    y <- getidx (parsename name)
    patt <- parsePatterns mn pat
    return $ PAs l NoTypeAnn ((y,parsename name),l) patt
parsePatterns _  _                         =
  error "parsePatterns"

-- | parses a statement
parseStms ::
  MonadState AHState m => MName -> TypeS a -> Stmt a -> m (Statement a)
parseStms str t (Generator l pat e) =
  do
    patt <- parsePatterns str pat
    expr <- parseExpr str t e
    return $ SPat l  patt expr
parseStms str t (Qualifier l e)     =
  do
    expr <- parseExpr str t e
    return $ SExpr expr
parseStms str t (LetStmt l binds)   =
  do
    bnd <- parseBinds str t binds
    return $ SLet l bnd
parseStms _  _ _                    =
  error "parseStms"

-- | Parses a binding
parseBinds ::
  MonadState AHState m => String -> TypeS a -> Binds a -> m [LocalDecl a]
parseBinds str t (BDecls _ decls) = parseFuncPatDecls str t decls
parseBinds _ _ _                  = return []

-- | Parses an alternative
parseAlternatives ::
  MonadState AHState m => MName -> TypeS a -> Alt a -> m (BranchExpr a)
parseAlternatives str t (Alt l pat rhs _) =
  do
    patt <- parsePatterns str pat
    rh <- rightHandtoExp str t rhs
    return $ Branch l patt rh

-- | Parses an QOp
parseQOp :: String -> QOp l -> AH.QName
parseQOp mn (QVarOp l qn) = (mn,parseQName qn)
parseQOp mn (QConOp l qn) = (mn, parseQName qn)

-- | Parses an qualified statement
parseQualsStms ::
  MonadState AHState m => MName -> TypeS a -> QualStmt a -> m (Statement a)
parseQualsStms str t (QualStmt _ stm ) = parseStms str t stm

-- | Parses a literal
parseLiteral :: HSE.Literal l -> AH.Literal
parseLiteral (Char _ c _)     = Charc c
parseLiteral (String _ str _) = Stringc str
parseLiteral (Int _ i _)      = Intc $ fromInteger i
parseLiteral (Frac _ r _)     = Floatc $ fromRational r
parseLiteral _                = error "parseLiteral"

-- | Parses a type
parseTyp :: MonadState AHState m => MName -> Type a -> m (TypeExpr a)
parseTyp _    (TyVar l name)          =
  do
    y <- getidx (parsename name)
    return $ TVar ((y, parsename name),l)
parseTyp modu (TyFun l t1 t2)         =
  do
    ty1 <- parseTyp modu t1
    ty2 <- parseTyp modu t2
    return $ FuncType l ty1 ty2
parseTyp modu (TyTuple l Boxed types) =
  do
    ty <- mapM (parseTyp modu) types
    return $ TCons l (tupleName (length types),l) ty
parseTyp modu (TyList l typ)          =
  do
    ty <- parseTyp modu typ
    return $ TCons l (("Prelude", "[]"),l) [(ty)]
parseTyp modu (TyCon l qname)         =
  return $ TCons l ((modu, parseQName qname),l) []
parseTyp modu (TyParen l t)           =
  parseTyp modu t
    --return $ TCons l ((modu, ""),l) [(ty)]
parseTyp modu  _                      =
  error "parseTyp"

-- | Parses the arity
parseArity :: [Match l] -> Int
parseArity []                                      = 0
parseArity ((InfixMatch {}):ms)                    = 2
parseArity (Match l name patterns rhs wbinds : ms) = length patterns

-- | Parses the importlist
parseImportList :: [ImportDecl l] -> [(MName, l)]
parseImportList [] = []
parseImportList (ImportDecl l name _ _ _ _ _ _ :xs) =
  (parseModuleName name,l) : parseImportList xs

-- | Parses the modulehead
parseModuleHead :: Maybe (ModuleHead l) -> MName
parseModuleHead Nothing                     = ""
parseModuleHead (Just (ModuleHead l n _ _)) = parseModuleName n

-- | parses the module name
parseModuleName :: ModuleName l -> String
parseModuleName (ModuleName l str) = str

-- | Parses decls
parseDecls :: [Decl l] -> TypeS l
parseDecls = Prelude.foldr ((++) . parseOneDecl) []

-- | Parses a single decl
parseOneDecl :: Decl l -> TypeS l
parseOneDecl (HSE.TypeSig l names t) = concatMap (parseTypeSig t) names
parseOneDecl _                       = []

-- | Parses a name
parsename :: Name l -> String
parsename (Ident l name)      = name
parsename (HSE.Symbol l name) = name

-- | Parses a qname
parseQName :: HSE.QName l -> String
parseQName (Qual l mdn name) = parsename name
parseQName (UnQual l name)   = parsename name
parseQName _                 = ""

-- | Parses a match name
parseMatchName :: Match l -> String
parseMatchName (Match l name patterns rhs wbinds)       = parsename name
parseMatchName (InfixMatch l pat1 name pat2 rhs wbinds) = parsename name

-- | Look of the type of a function is defined in the module
searchForType :: String -> TypeS l -> Maybe (Type l)
searchForType _ [] = Nothing
searchForType name ((n , t):nts) | name == (parsename n) =  Just t
                                 | otherwise = searchForType name nts

-- | Generates a typesignature
buildType :: MonadState AHState m => a -> MName -> Type a -> m (TypeSig a)
buildType l modname x          = do
                                   t <- parseTyp modname x
                                   return $ AH.TypeSig t

-- | Returns the name to a ah pattern
parseNamePattern :: Pattern l -> AH.QName
parseNamePattern (AH.PVar t ((i,s),l))   = ("",s)
parseNamePattern (PComb l t (qn,a) pats) = qn
parseNamePattern (PAs a t ((i,s),l) pat) = ("",s)
parseNamePattern (AH.PLit t (l, a))      = ("", "Literal")
parseNamePattern (AH.PTuple a t pats)    = ("","Tupel")
parseNamePattern (AH.PList a t pats)     = ("","List")

-- | Returns the name to a hse pattern
parseNameOutOfPattern :: Pat l -> String
parseNameOutOfPattern (HSE.PVar l name)         =
  parsename name
parseNameOutOfPattern (HSE.PTuple l Boxed pats) =
  "Tupel " ++ concatMap parseNameOutOfPattern pats
parseNameOutOfPattern (HSE.PList l pats)        =
  "Liste " ++ concatMap parseNameOutOfPattern pats
parseNameOutOfPattern (PParen _ pat)            =
  parseNameOutOfPattern pat
parseNameOutOfPattern (PAsPat l name pat)       =
  parsename name

  -- | For functions declarations only
  --   FunBind,
  --   Patbind,
  --   are allowed

filterFunDecls :: [Decl l] -> [Decl l]
filterFunDecls []                                 = []
filterFunDecls (x@(FunBind l mas@(m:matches)):xs) = x:filterFunDecls xs
filterFunDecls (x@PatBind{} : xs)                 = x:filterFunDecls xs
filterFunDecls (x:xs)                             = filterFunDecls xs

-- | For typedeclarations only
--   TypeDecl,
--   DataDecls as DataType,
--   are allowed
filterdecls :: [Decl l] -> [Decl l]
filterdecls []                                             = []
filterdecls (x@HSE.TypeDecl{} : xs)      = x : filterdecls xs
filterdecls (x@(DataDecl _ (DataType _) Nothing _ _ _):xs) = x : filterdecls xs
filterdecls (x:xs)                                         = filterdecls xs

-- | For qualified constructer declarations only
--   QualConDecl
--   is allowed
filterqual :: [QualConDecl l] -> [QualConDecl l]
filterqual []                                    = []
filterqual (x@(QualConDecl _ (Just tvb) _ _):xs) = x : filterqual xs
filterqual (x:xs)                                = filterqual xs

-- | For Statements here is only
--   Qualifier
--   allowed
filterStmts :: [Stmt a] -> [Stmt a]
filterStmts []                        = []
filterStmts (x@(Qualifier l expr):xs) = x : filterStmts xs
filterStmts (x:xs)                    = filterStmts xs

-- | For qualified statements only
--   QualStmt
--   is allowed
filterQualsStmts :: [QualStmt l] -> [QualStmt l]
filterQualsStmts []                       = []
filterQualsStmts (x@(QualStmt _ stm ):xs) = x : filterQualsStmts xs
filterQualsStmts (x:xs)                   = filterQualsStmts xs

------------------------------------------------------------------------------

start modu = do
  let (a,b) = runState (getFunctionNames modu) initialState
  error $ show a
  return a

getFunctionNames :: MonadState AHState m => Module l -> m [String]
getFunctionNames (Module l mdh mdP imps decl) =
  do
   mapM getFunctionNamesDecls decl
   ahs <- get
   return $ fctNames ahs

getFunctionNamesDecls (FunBind l matches)               =
  do
    mapM getFunctionNamesMatches matches
    return ()
getFunctionNamesDecls (PatBind l pat rhs mbinds)        =
  do
    case mbinds of
      Nothing -> do
                   getFunctionNamesPats pat
                   getFunctionNamesRhs rhs
                   return ()
      Just mb -> do
                   getFunctionNamesPats pat
                   getFunctionNamesRhs rhs
                   getFunctionNamesBinds mb
                   return ()
getFunctionNamesDecls _                                 = return ()

getFunctionNamesMatches (Match l name pats rhs mbinds)          =
  do
    getFunctionNamesRhs rhs
    case mbinds of
      Just mb -> do
                   getFunctionNamesBinds mb
                   ahs <- get
                   put AHState {idx = idx ahs, vmap = vmap ahs, fctNames = fctNames ahs ++ [parsename name]}
                   return ()
      Nothing -> do
                   ahs <- get
                   put AHState {idx = idx ahs, vmap = vmap ahs, fctNames = fctNames ahs ++ [parsename name]}
                   return ()
getFunctionNamesMatches (InfixMatch l pat name pats rhs mbinds) =
  do
    getFunctionNamesRhs rhs
    case mbinds of
      Just mb -> do
                   getFunctionNamesBinds mb
                   ahs <- get
                   put AHState {idx = idx ahs, vmap = vmap ahs,
                   fctNames = fctNames ahs ++ [parsename name]}
                   return ()
      Nothing -> do
                   ahs <- get
                   put AHState {idx = idx ahs, vmap = vmap ahs,
                   fctNames = fctNames ahs ++ [parsename name]}
                   return ()

getFunctionNamesBinds (BDecls l decls) =
  do
    mapM getFunctionNamesDecls decls
    return ()
getFunctionNamesBinds _                =
  return ()

getFunctionNamesRhs (UnGuardedRhs l expr)          =
  do
    getFunctionNamesExpr expr
    return ()
getFunctionNamesRhs _                              =
  return ()

getFunctionNamesExpr (HSE.Let l binds expr) =
  do
    getFunctionNamesBinds binds
    return ()
getFunctionNamesExpr (Do l stmts)           =
  do
    mapM getFunctionNamesStmts stmts
    return ()
getFunctionNamesExpr _                      =
  return ()

getFunctionNamesPats pat =
  do
    let fn = parseNameOutOfPattern pat
    ahs <- get
    put AHState {idx = idx ahs, vmap = vmap ahs, fctNames = fctNames ahs ++ [fn]}
    return ()
getFunctionNamesStmts (Qualifier l expr)=
  do
    getFunctionNamesExpr expr
    return ()
getFunctionNamesStmts (LetStmt l binds) =
  do
    getFunctionNamesBinds binds
    return ()
getFunctionNamesStmts _                 =
  return ()

parseFile' :: FilePath -> IO (Module SrcSpanInfo)
parseFile' f = do
          (ParseOk ast) <- parseFile f
          return ast
