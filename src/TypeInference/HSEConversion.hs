{-# LANGUAGE FlexibleContexts      #-}

module TypeInference.HSEConversion
  ( hseToNLAH, parseNamePattern, findDifference,hseExpToAHExpr,start
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
                       , fctNames :: [AH.QName]
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
                   put AHState {idx= idx' +1
                               ,vmap=insert name idx' $ vmap ahs
                               ,fctNames = fctNames ahs
                               }
                   return idx'

-------------------------------------------------------------------------------
-- TRANSFORMATION HSE TO AH  --------------------------------------------------
-------------------------------------------------------------------------------

-- | Transforms a haskell-src-extensions module into an abstract haskell
--   program
hseToNLAH :: DML.Map AH.QName (TypeExpr a) -> Module a -> Prog a
hseToNLAH mapTE modu = evalState (astToAbstractHaskell mapTE modu) initialState

hseExpToAHExpr :: Map AH.QName a -> Exp a1 -> Expr a1
hseExpToAHExpr mapTE expr = evalState (astExprToAbstractHaskellExpr mapTE expr) initialState

astExprToAbstractHaskellExpr :: MonadState AHState m => Map AH.QName a -> Exp a1 -> m (Expr a1)
astExprToAbstractHaskellExpr mapTE expr =
  do
    ahs <- get
    let qNamesMap = keys mapTE
    put AHState {idx = idx ahs, vmap = vmap ahs, fctNames = fctNames ahs ++ qNamesMap}
    exprNew <- parseExpr "" [] expr
    return exprNew

astToAbstractHaskell ::
  MonadState AHState m => Map AH.QName a -> Module a1 -> m (Prog a1)
astToAbstractHaskell mapTE modu@(Module l modh mp imps declas) =
  do
    let mn = parseModuleHead modh
    getFunctionNames mn modu
    st <- get
    let qNamesMap = keys mapTE
    let allFunctionNames = qNamesMap ++ fctNames st
    put AHState {idx = idx st, vmap = vmap st, fctNames = fctNames st ++ allFunctionNames}
    let ts = parseTypeSignatur modu
    let il = parseImportList imps
    tdcl <- mapM (parseTypDecls mn) $ filterdecls declas
    fdcl <- mapM (parseFunDecls mn ts) $ filterFunDecls declas
    return $ Prog (mn,l) il tdcl fdcl
astToAbstractHaskell _ _ = return $ Prog ("",undefined) [] [] []

-- | Returns the difference of two lists
findDifference :: Eq a => [a] -> [a] -> [a]
findDifference xs ys = [y | y <- ys, y `notElem` xs]

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
    --etv <- mapM  extractTypvariables $ filterqual qualcondecls
    ev <- evt declhead
  --  let he = concat etv
    qcd <- mapM (parseQualConDecl str) qualcondecls
    return $ Type l (parseTypeName str declhead,l) Public ev qcd


evt :: MonadState AHState m => DeclHead a -> m ([(VarName,a)])
evt (DHead l name)              = return []
evt (DHInfix l tyVarBind name)  = parseTVB tyVarBind
evt (DHParen l declhead)        = evt declhead
evt (DHApp l declhead tyVarBind) =
  do
    e1 <- evt declhead
    e2 <- parseTVB tyVarBind
    return $ e1 ++ e2

-- | Parses a typevariable
parseTVB :: MonadState AHState m => TyVarBind l -> m [((Int, String), l)]
parseTVB (KindedVar l name _) = do
                                  y <- getidx (parsename name)
                                  return [((y, parsename name),l)]
parseTVB (UnkindedVar l name) = do
                                  y <- getidx (parsename name)
                                  return [((y, parsename name),l)]

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
    let s = searchForType pn ts
    rl <- parseRulesOutOfPats modu ts pat rhs mbinds
    case s of
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
   MonadState AHState m => MName ->
                           TypeS a ->
                           t ->
                           HSE.Rhs a ->
                           Maybe (Binds a) ->
                           m (Rules a)
parseRulesOutOfPats modu ts pat rhs mbinds =
  do
    case mbinds of
      Nothing -> do
                   rs <- parseRule modu ts [] rhs
                   return $ Rules [rs]
      Just x@(BDecls _ decs) -> do
                                (AH.Rule a b c d e) <- parseRule modu ts [] rhs
                                ls <- parseBinds modu ts x
                                return $ Rules [AH.Rule a b c d ls]

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
  MonadState AHState m => String -> TypeS a -> Decl a -> m (LocalDecl a)
parseFuncPatDecls str t (FunBind l matches@(m:ms))       =
  do
    let fn = parseMatchName  m
    case (searchForType fn t) of
      Nothing -> do
                   rl <- mapM (parseRules str t) matches
                   let r = Rules rl
                   let fname = ((str,fn),l)
                   let ar = parseArity matches
                   return $ LocalFunc $ Func l fname ar Public Untyped r
      Just z -> do
                  btd <- buildType l str z
                  rl <- mapM (parseRules str t) matches
                  let r = Rules rl
                  let fname = ((str,fn),l)
                  let ar = parseArity matches
                  return $ LocalFunc $ Func l fname ar Public btd r
parseFuncPatDecls str t
  (PatBind l pat rhs@(UnGuardedRhs a expr) (Just wbind)) =
   do
    rh <- parseExprOutOfRhs str t rhs
    patt <- parsePatterns str pat
    bnd <- parseBinds str t wbind
    return $ LocalPat l patt rh bnd
parseFuncPatDecls str t
  (PatBind l pat rhs@(GuardedRhss a gds) (Just b)) |length gds <= 1 =
   do
     rh <- parseExprOutOfGrd str t (head gds)
     patt <- parsePatterns str pat
     bnd <- parseBinds str t b
     return $ LocalPat l patt rh bnd
                                                        |otherwise      =
   do
     let name = parseNameOutOfPattern pat
     rh <- parseExprOutOfGrd str t (head gds)
     patt <- parsePatterns str pat
     bnd <- parseBinds str t b
     rules <- mapM (parseRulesOutOfGuarded str t) gds
     let rulesR = makeRules l rules
     let n = newRule l patt rulesR bnd
     return $ LocalFunc $ Func l (("",name),l) 0 Public Untyped (Rules [n])
parseFuncPatDecls str t
  (PatBind l pat rhs@(GuardedRhss a gds) Nothing) |length gds <= 1 =
   do
     rh <- parseExprOutOfGrd str t (head gds)
     patt <- parsePatterns str pat
     return $ LocalPat l patt rh []
                                                        |otherwise      =
   do
     let name = parseNameOutOfPattern pat
     rh <- parseExprOutOfGrd str t (head gds)
     patt <- parsePatterns str pat
     rules <- mapM (parseRulesOutOfGuarded str t) gds
     let rulesR = makeRules l rules
     let n = newRule l patt rulesR []
     return $ LocalFunc $ Func l (("",name),l) 0 Public Untyped (Rules [n])
parseFuncPatDecls str t
  (PatBind l pat rhs@(UnGuardedRhs a expr) Nothing) =
   do
    rh <- parseExprOutOfRhs str t rhs
    patt <- parsePatterns str pat
    return $ LocalPat l patt rh []

-- | Builds a new Rule
newRule :: a -> Pattern a -> AH.Rhs a -> [LocalDecl a] -> AH.Rule a
newRule l pat rhs b = AH.Rule l NoTypeAnn [pat] rhs b

-- | Makes one right hand side out of a list of right hand sides
makeRules :: a -> [AH.Rhs a] -> AH.Rhs a
makeRules l xs = let r =  makeRules' xs
                  in SimpleRhs $ AH.List l NoTypeAnn r

-- | transforms tupel into a list
toExprList :: [(t, t)] -> [t]
toExprList  []        =[]
toExprList ((a,b):xs) = a: b: toExprList xs

makeRules' :: [AH.Rhs t] -> [Expr t]
makeRules' ((AH.GuardedRhs a es):xs) = (toExprList es) ++ makeRules' xs

parseRulesOutOfGuarded ::
   MonadState AHState m => MName -> TypeS a -> GuardedRhs a -> m (AH.Rhs a)
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
    case  qn  of
      (Qual _ mon name) -> case elem (parseModuleName mon,parsename name) (fctNames ahs) of
                             True -> return $ AH.Symbol NoTypeAnn (parseSpecialQNameNew (parseModuleName mon) qn,l)
                             False -> do
                                       y <- getidx (parseQName qn)
                                       return $  AH.Var NoTypeAnn ((y,parseQName qn),l)

      (UnQual l name) -> case elem (mn,parsename name) (fctNames ahs) of
                           True -> return $ AH.Symbol NoTypeAnn (parseSpecialQNameNew mn qn,l)
                           False -> do
                             case elem ("Prelude",parsename name) (fctNames ahs) of
                               True -> return $ AH.Symbol NoTypeAnn (parseSpecialQNameNew "Prelude" qn,l)
                               False ->
                                 case (parsename name) of
                                   "otherwise" -> return $ AH.Symbol NoTypeAnn (parseSpecialQNameNew "Prelude" qn,l)
                                   _ -> do
                                          y <- getidx (parseQName qn)
                                          return $  AH.Var NoTypeAnn ((y,parseQName qn),l)
parseExpr mn _ (Con l qn)                        =
   return $ AH.Symbol NoTypeAnn (parseSpecialQNameNew mn qn,l)
parseExpr _  _ (HSE.Lit l lit)                   =
  return $ AH.Lit NoTypeAnn (parseLiteral lit, l)
parseExpr mn t (InfixApp l exp1 qop exp2)        =
  do
    expr1 <- parseExpr mn t exp1
    expr2 <- parseExpr mn t exp2
    qop' <- parseQOp mn qop
    return $ InfixApply l NoTypeAnn expr1 (qop', l) expr2
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
parseExpr mn t (TupleSection l b xs)             =
  do
    exprs <- parseMaybeExpr mn t xs
    return $ AH.Tuple l NoTypeAnn  exprs
parseExpr mn t (LeftSection l expr qop)          =
  do
    e1 <- parseExpr mn t expr
    q <- parseQOp mn qop
    let e2 =  AH.Var NoTypeAnn ((-1,"error"), l)
    return $ AH.Lambda l NoTypeAnn [AH.PVar NoTypeAnn ((-1,"error"), l)] (InfixApply l NoTypeAnn e1 (q,l) e2)
parseExpr mn t (RightSection l qop expr)         =
  do
    e1 <- parseExpr mn t expr
    q <- parseQOp mn qop
    let e2 =  AH.Var NoTypeAnn ((-1,"error"), l)
    return $ AH.Lambda l NoTypeAnn [AH.PVar NoTypeAnn ((-1,"error"), l)] (InfixApply l NoTypeAnn e2 (q,l) e1)

parseMaybeExpr :: MonadState AHState m => MName -> TypeS a -> [Maybe (Exp a)] -> m [Expr a]
parseMaybeExpr mn t [] = return []
parseMaybeExpr mn t (Nothing:xs) = parseMaybeExpr mn t xs
parseMaybeExpr mn t ((Just x):xs)=
  do
    f <- parseMaybeExpr mn t xs
    l <- parseExpr mn t x
    return (l:f)

-- | Transforms a right hand side to an expr
rightHandtoExp ::
  MonadState AHState m => MName -> TypeS a -> HSE.Rhs a -> m (Expr a)
rightHandtoExp str t (UnGuardedRhs _ e)    = parseExpr str t e
rightHandtoExp str t (GuardedRhss _ gdrhs) = error "rightHandtoExp"

-- | Parses a pattern
parsePatterns:: MonadState AHState m => MName -> Pat l -> m (Pattern l)
parsePatterns mn (HSE.PVar l name)              =
  do
    y <- getidx (parsename name)
    return $ AH.PVar NoTypeAnn ((y,parsename name),l)
parsePatterns mn (HSE.PLit l sign lit)          =
  return $ AH.PLit NoTypeAnn (parseLiteral lit, l)
parsePatterns mn (PApp l qn pats)               =
  do
    pat <- mapM (parsePatterns mn) pats
    return $ PComb l NoTypeAnn (parseSpecialQNameNew mn qn,l) pat
parsePatterns mn (HSE.PTuple l Boxed pats)      =
  do
    pat <- mapM (parsePatterns mn) pats
    return $ AH.PTuple l NoTypeAnn pat
parsePatterns mn (HSE.PList l pats)             =
  do
    pat <- mapM (parsePatterns mn) pats
    return $ AH.PList l NoTypeAnn pat
parsePatterns mn (PParen _ pat)                 =
  parsePatterns mn pat
parsePatterns mn (PAsPat l name pat)            =
  do
    y <- getidx (parsename name)
    patt <- parsePatterns mn pat
    return $ AH.PAs l NoTypeAnn ((y,parsename name),l) patt
parsePatterns mn (HSE.PInfixApp l pat1 qn pat2) =
  do
    pa1 <- parsePatterns mn pat1
    pa2 <-parsePatterns mn pat2
    return $ PComb l NoTypeAnn (parseSpecialQNameNew mn qn,l) [pa1,pa2]
parsePatterns mn (PWildCard l)                  =
  do
    y <- getidx ("")
    return $ AH.PVar NoTypeAnn ((y,""),l)
parsePatterns _  _                              =
  error "parsePatterns"

parseSpecialQNameNew :: String -> HSE.QName a -> AH.QName
parseSpecialQNameNew mn a@(UnQual l (Ident _ "True")) = parseQNameNew "Prelude" a
parseSpecialQNameNew mn a@(UnQual l (Ident _ "False"))= parseQNameNew "Prelude" a
parseSpecialQNameNew mn a@(UnQual l (Ident _ "Nothing"))=parseQNameNew "Prelude" a
parseSpecialQNameNew mn a@(UnQual l (Ident _"Just"))    =parseQNameNew "Prelude" a
parseSpecialQNameNew mn a@(UnQual l (Ident _ "Left"))   = parseQNameNew "Prelude" a
parseSpecialQNameNew mn a@(UnQual l (Ident _ "Right"))  = parseQNameNew "Prelude" a
parseSpecialQNameNew mn (Special l (HSE.Cons g))        = ("Prelude","(:)")
parseSpecialQNameNew mn (Special l (UnboxedSingleCon g)) = (tupleName 2)
parseSpecialQNameNew mn (Special l (TupleCon g b i))     = (tupleName i)
parseSpecialQNameNew mn (Special l (UnitCon g))          = ("Prelude", "()")
parseSpecialQNameNew mn (Special l (ListCon g))          = ("Prelude", "[]")
parseSpecialQNameNew mn (Special l (FunCon g ))          = ("Prelude", "->")
parseSpecialQNameNew mn x                                = parseQNameNew mn x

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

-- | Parses a binding
parseBinds ::
  MonadState AHState m => String -> TypeS a -> Binds a -> m [LocalDecl a]
parseBinds str t (BDecls _ decls) = mapM (parseFuncPatDecls str t) decls
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
parseQOp :: MonadState AHState m => MName -> QOp a -> m AH.QName
parseQOp mn (QVarOp l qn) =
   do
     ahs <- get
     case elem (mn,parseQName qn) (fctNames ahs) of
       True -> return $ parseSpecialQNameNew mn qn
       False -> case elem ("Prelude",parseQName qn) (fctNames ahs) of
         True -> return $ parseSpecialQNameNew "Prelude" qn
         False -> return $ parseSpecialQNameNew mn qn
parseQOp mn (QConOp l qn) =
  do
    ahs <- get
    case elem (mn,parseQName qn) (fctNames ahs) of
      True -> return $ parseSpecialQNameNew mn qn
      False -> case elem ("Prelude",parseQName qn) (fctNames ahs) of
        True -> return $ parseSpecialQNameNew "Prelude" qn
        False -> return $ parseSpecialQNameNew mn qn

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
parseTyp _    (TyVar l name)             =
  do
    y <- getidx (parsename name)
    return $ TVar ((y, parsename name),l)
parseTyp modu (TyFun l t1 t2)            =
  do
    ty1 <- parseTyp modu t1
    ty2 <- parseTyp modu t2
    return $ FuncType l ty1 ty2
parseTyp modu (TyTuple l Boxed types)    =
  do
    ty <- mapM (parseTyp modu) types
    return $ TCons l (tupleName (length types),l) ty
parseTyp modu (TyList l typ)             =
  do
    ty <- parseTyp modu typ
    return $ TCons l (("Prelude", "[]"),l) [(ty)]
parseTyp modu (TyCon l qname)            =
  do
   case parseQName qname of
     "Int"   -> return $ TCons l (("Prelude","Int"),l) []
     "Maybe" -> return $ TCons l (("Prelude", "Maybe"),l) []
     "Bool"  -> return $ TCons l (("Prelude","Bool"),l) []
     "String"-> return $ TCons l (("Prelude", "String"),l) []
     "Char"  -> return $ TCons l (("Prelude", "Char"),l) []
     "Float" -> return $ TCons l (("Prelude","Float"),l) []
     _ -> return $ TCons l (parseSpecialQNameNew modu qname,l) []
parseTyp modu (TyParen l t)              =
  parseTyp modu t
parseTyp modu (TyApp l t1 t2)            =
  do
    (TCons _ _ args) <- parseTyp modu t1
    t2p <- parseTyp modu t2
    return $ TCons l ((parseTypName modu t1),l) (args ++ [t2p])
    --return $ FuncType l t1p t2p
parseTyp modu (TyWildCard l (Just name)) =
  return $ TCons l ((modu,parsename name), l) []
parseTyp modu (TyWildCard l Nothing)     =
  return $ TCons l ((modu,""), l) []
parseTyp modu (TyForall l mtv mc t)      = do
  parseTyp modu t

parseTypName :: String -> Type a -> AH.QName
parseTypName modu (TyVar l name)  = (modu,parsename name)
parseTypName modu (TyTuple l Boxed types) =  (tupleName (length types))
parseTypName modu (TyList l typ)          = ("Prelude","[]")
parseTypName modu (TyCon l qname)         =
   case parseQName qname of
     "Int"   -> ("Prelude","Int")
     "Maybe" -> ("Prelude", "Maybe")
     "Bool"  -> ("Prelude","Bool")
     "String"->("Prelude", "String")
     "Char"  -> ("Prelude", "Char")
     "Float" -> ("Prelude","Float")
     _ -> (parseSpecialQNameNew modu qname)
parseTypName modu (TyParen l t)           =
  parseTypName modu t
parseTypName modu (TyApp l t1 t2) = parseTypName modu t1

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

parseQNameNew modu x@(Qual l (ModuleName d mdn) name) =(mdn, parsename name)
parseQNameNew modu (UnQual l name)   =(modu, parsename name)
parseQNameNew modu _                 = ("","")

-- | Parses a match name
parseMatchName :: Match l -> String
parseMatchName (Match l name patterns rhs wbinds)       = parsename name
parseMatchName (InfixMatch l pat1 name pat2 rhs wbinds) = parsename name

-- | Looks if the type of a function is defined in the module
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
parseNameOutOfPattern (HSE.PVar l name)              =
  parsename name
parseNameOutOfPattern (HSE.PLit l sign lit)          =
  parseNameOutOfLiteral lit
parseNameOutOfPattern (HSE.PTuple l Boxed pats)      =
  "Tupel " ++ concatMap parseNameOutOfPattern pats
parseNameOutOfPattern (HSE.PList l pats)             =
  "Liste " ++ concatMap parseNameOutOfPattern pats
parseNameOutOfPattern (PParen _ pat)                 =
  parseNameOutOfPattern pat
parseNameOutOfPattern (PAsPat l name pat)            =
  parsename name
parseNameOutOfPattern (PApp l qname pats)            =
  parseQName qname
parseNameOutOfPattern (HSE.PInfixApp l pat1 qn pat2) =
  (parseNameOutOfPattern pat1) ++ parseQName qn ++ (parseNameOutOfPattern pat2)
parseNameOutOfPattern (HSE.PWildCard l)              =
  "_"
parseNameOutOfPattern _ = error "parseNameOutOfPattern"

parseNameOutOfLiteral :: HSE.Literal l -> String
parseNameOutOfLiteral (Char _ _ s)    = s
parseNameOutOfLiteral (String _ _ s)  = s
parseNameOutOfLiteral (Int _ _ s)     = s
parseNameOutOfLiteral (Frac _ _ s)     = s
parseNameOutOfLiteral _ = error "parseNameOutOfLiteral"

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
start :: Monad m => String -> Module l -> m [AH.QName]
start mn modu = do
  let (a,b) = runState (getFunctionNames mn modu) initialState
  return a

getFunctionNames :: MonadState AHState m =>String -> Module l -> m [AH.QName]
getFunctionNames modu (Module l mdh mdP imps decl) =
  do
   mapM (getFunctionNamesDecls modu) decl
   ahs <- get
   let f = fctNames ahs
   let newFctNames = filterFctNames f
   let ndNewFct = rmDup newFctNames
   return $ ndNewFct--fctNames ahs

rmDup :: Eq a => [a] -> [a]
rmDup [] = []
rmDup (x:xs) = x : rmDup (Prelude.filter (\y -> not(x == y)) xs)

filterFctNames :: [AH.QName] -> [AH.QName]
filterFctNames [] = []
filterFctNames ((x,"True"):xs) = filterFctNames xs
filterFctNames ((x,"False"):xs)= filterFctNames xs
filterFctNames ((x,"Just"):xs) = filterFctNames xs
filterFctNames ((x,"Nothing"):xs)=filterFctNames xs
filterFctNames ((x,"Left"):xs)=filterFctNames xs
filterFctNames ((x,"Right"):xs)=filterFctNames xs
filterFctNames (y:xs) = y:filterFctNames xs

getFunctionNamesDecls :: MonadState AHState m => String -> Decl l -> m ()
getFunctionNamesDecls modu (FunBind l matches)               =
  do
    mapM (getFunctionNamesMatches modu) matches
    return ()
getFunctionNamesDecls modu (PatBind l pat rhs mbinds)        =
  do
    case mbinds of
      Nothing -> do
                   getFunctionNamesPats modu pat
                   getFunctionNamesRhs modu rhs
                   return ()
      Just mb -> do
                   getFunctionNamesPats modu pat
                   getFunctionNamesRhs modu rhs
                   getFunctionNamesBinds modu mb
                   return ()
getFunctionNamesDecls _  _                               =
  return ()

getFunctionNamesMatches :: MonadState AHState m =>String -> Match l -> m ()
getFunctionNamesMatches modu (Match l name pats rhs mbinds)          =
  do
    getFunctionNamesRhs modu rhs
    case mbinds of
      Just mb -> do
                   getFunctionNamesBinds modu mb
                   ahs <- get
                   put AHState {idx = idx ahs, vmap = vmap ahs, fctNames = fctNames ahs ++ [(modu ,parsename name)]}
                   return ()
      Nothing -> do
                   ahs <- get
                   put AHState {idx = idx ahs, vmap = vmap ahs, fctNames = fctNames ahs ++ [(modu,parsename name)]}
                   return ()
getFunctionNamesMatches modu (InfixMatch l pat name pats rhs mbinds) =
  do
    getFunctionNamesRhs modu rhs
    case mbinds of
      Just mb -> do
                   getFunctionNamesBinds modu mb
                   ahs <- get
                   put AHState {idx = idx ahs, vmap = vmap ahs,
                   fctNames = fctNames ahs ++ [(modu,parsename name)]}
                   return ()
      Nothing -> do
                   ahs <- get
                   put AHState {idx = idx ahs, vmap = vmap ahs,
                   fctNames = fctNames ahs ++ [(modu,parsename name)]}
                   return ()

getFunctionNamesBinds :: MonadState AHState m => String -> Binds l -> m ()
getFunctionNamesBinds modu (BDecls l decls) =
  do
    let cbd = checkDecls decls
    mapM (getFunctionNamesDecls modu) cbd
    return ()
getFunctionNamesBinds _ _               =
  return ()

checkDecls :: [Decl l] -> [Decl l]
checkDecls [] = []
checkDecls ((PatBind _ _ _ _):xs) = checkDecls xs
checkDecls (x:xs) = x:checkDecls xs

getFunctionNamesRhs :: MonadState AHState m => String -> HSE.Rhs l -> m ()
getFunctionNamesRhs modu (UnGuardedRhs l expr)          =
  do
    getFunctionNamesExpr modu expr
    return ()
getFunctionNamesRhs _ _                             =
  return ()

getFunctionNamesExpr :: MonadState AHState m => String -> Exp l -> m ()
getFunctionNamesExpr modu (HSE.Let l binds expr) =
  do
    getFunctionNamesBinds modu binds
    return ()
getFunctionNamesExpr modu (Do l stmts)           =
  do
    mapM (getFunctionNamesStmts modu) stmts
    return ()
getFunctionNamesExpr _   _                   =
  return ()

getFunctionNamesPats :: MonadState AHState m => String -> Pat l -> m ()
getFunctionNamesPats modu pat                =
  do
    let fn = parseNameOutOfPattern pat
    ahs <- get
    put AHState {idx = idx ahs, vmap = vmap ahs, fctNames = fctNames ahs ++ [(modu,fn)]}
    return ()

getFunctionNamesStmts :: MonadState AHState m => String -> Stmt l -> m ()
getFunctionNamesStmts modu (Qualifier l expr)=
  do
    getFunctionNamesExpr modu expr
    return ()
getFunctionNamesStmts modu (LetStmt l binds) =
  do
    getFunctionNamesBinds modu binds
    return ()
getFunctionNamesStmts _  _                   =
  return ()

parseFile' :: FilePath -> IO (Module SrcSpanInfo)
parseFile' f = do
          (ParseOk ast) <- parseFile f
          return ast
