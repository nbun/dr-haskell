{-|
  This is the main library for type inference of abstract Haskell programs. It
  can also be used to infer Haskell programs with the
  'Language.Haskell.Exts.Syntax' representation.
-}

module TypeInference.Main
  ( TIError (..)
  , inferProg, inferFuncDecl, inferExpr, inferHSE
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState, get, modify, put)
import Data.List (find)
import qualified Data.Map as DM
import Data.Maybe (catMaybes, fromJust)
import Goodies ((++=), both, bothM, mapAccumM, one, two)
import Language.Haskell.Exts (Module)
import TypeInference.AbstractHaskell
import TypeInference.AbstractHaskellGoodies
import TypeInference.Normalization (normalize, normFuncDecl, normExpr)
import TypeInference.Term (Term (..), TermEqs)
import TypeInference.TypeSubstitution (TESubst, applyTESubstFD, applyTESubstE)
import TypeInference.Unification (UnificationError (..), unify)

-- -----------------------------------------------------------------------------
-- Representation of type environments
-- -----------------------------------------------------------------------------

-- | A type environment represented as a map from qualified names to type
--   expressions and parameterized over the type of annotations.
type TypeEnv a = DM.Map QName (TypeExpr a)

-- -----------------------------------------------------------------------------
-- Functions for type environments
-- -----------------------------------------------------------------------------

-- | The empty type environment.
emptyTypeEnv :: TypeEnv a
emptyTypeEnv = DM.empty

-- | Returns the type expression mapped to the given qualified name or 'Nothing'
--   if no such mapping exists.
lookupType :: QName -> TypeEnv a -> Maybe (TypeExpr a)
lookupType = DM.lookup

-- | Extends a type environment with a new mapping from the given qualified name
--   to the given type expression. An already existing mapping with the same
--   qualified name will be thrown away.
insertType :: QName -> TypeExpr a -> TypeEnv a -> TypeEnv a
insertType = DM.insert

-- | Returns a type environment that contains all the mappings from the given
--   list. For multiple mappings with the same qualified name, the last
--   corresponding mapping of the given list is taken.
listToTypeEnv :: [(QName, TypeExpr a)] -> TypeEnv a
listToTypeEnv = DM.fromList

-- -----------------------------------------------------------------------------
-- Representation of type inference states
-- -----------------------------------------------------------------------------

-- | A type inference state represented as a state consisting of a type
--   environment, an integer value for the next free type variable, a type
--   signature environment and a mapping from variables to type variables, and
--   parameterized over the type of annotations.
type TIState a = State (TypeEnv a, Int, TypeEnv a, DM.Map Int (TypeExpr a))

-- -----------------------------------------------------------------------------
-- Functions for type inference states
-- -----------------------------------------------------------------------------

-- | Returns an initial type inference state with the given type environment.
initTIState :: TypeEnv a -> (TypeEnv a, Int, TypeEnv a, DM.Map Int (TypeExpr a))
initTIState tenv = (tenv, 0, emptyTypeEnv, DM.empty)

-- -----------------------------------------------------------------------------
-- Representation of type inference monads
-- -----------------------------------------------------------------------------

-- | A type inference monad represented as a type inference state monad with an
--   additional type inference error, and parameterized over the type of
--   annotations.
type TIMonad a = ExceptT (TIError a) (TIState a)

-- -----------------------------------------------------------------------------
-- Representation of type inference errors
-- -----------------------------------------------------------------------------

-- | Representation of a type inference error, parameterized over the type of
--   annotations.
data TIError a = TIError String
               | TIClash (TypeExpr a) (TypeExpr a)
               | TIOccurCheck VarName (TypeExpr a)
               | TITooGeneral (TypeExpr a) (TypeExpr a)
  deriving Show

-- -----------------------------------------------------------------------------
-- Functions for interfacing with the unification module
-- -----------------------------------------------------------------------------

-- | Converts the given type expression into a term representation.
fromTypeExpr :: TypeExpr a -> Term QName [a]
fromTypeExpr (TVar ((v, _), x))    = TermVar [x] v
fromTypeExpr (FuncType x t1 t2)
  = TermCons [x] (preName "->") [fromTypeExpr t1, fromTypeExpr t2]
fromTypeExpr (TCons x (qn, y) tes) = TermCons [x, y] qn (map fromTypeExpr tes)

-- | Converts the given term representation into a type expression.
toTypeExpr :: Term QName [a] -> TypeExpr a
toTypeExpr (TermVar [x] v)         = teVar v x
toTypeExpr (TermCons (x:xs) qn ts)
  | snd qn == "->" && two ts
    = FuncType x (toTypeExpr (ts !! 0)) (toTypeExpr (ts !! 1))
  | one xs = TCons x (qn, head xs) (map toTypeExpr ts)
toTypeExpr _
  = error "The given term can not be converted into a type expression!"

-- | Converts the given list of type expression equations into a list of term
--   equations.
fromTypeExprEqs :: TypeExprEqs a -> TermEqs QName [a]
fromTypeExprEqs = map (both fromTypeExpr)

-- | Converts the given list of term equations into a list of type expression
--   equations.
toTypeExprEqs :: TermEqs QName [a] -> TypeExprEqs a
toTypeExprEqs = map (both toTypeExpr)

-- | Solves the given list of type expression equations.
solve :: TypeExprEqs a -> TIMonad a (TESubst a)
solve eqs = case unify (fromTypeExprEqs eqs) of
              Left e    -> throwError (toTIError e)
              Right sub -> return (DM.map toTypeExpr sub)
  where
    toTIError :: UnificationError QName [a] -> TIError a
    toTIError (Clash t1 t2)    = TIClash (toTypeExpr t1) (toTypeExpr t2)
    toTIError (OccurCheck v t) = TIOccurCheck (v, varToString v) (toTypeExpr t)

-- -----------------------------------------------------------------------------
-- Functions for type inference monads
-- -----------------------------------------------------------------------------

-- | Returns the next free type variable with the given annotation.
nextTVar :: a -> TIMonad a (TypeExpr a)
nextTVar x = do (tenv, n, tsenv, vsub) <- get
                put (tenv, n + 1, tsenv, vsub)
                return (teVar n x)

-- | Resets the mapping from variables to type variables in the type inference
--   state.
initVarTypes :: TIMonad a ()
initVarTypes = modify (\(tenv, n, tsenv, _) -> (tenv, n, tsenv, DM.empty))

-- | Resets the type signature environment in the type inference state.
initSigEnv :: TIMonad a ()
initSigEnv = modify (\(tenv, n, _, vsub) -> (tenv, n, emptyTypeEnv, vsub))

-- | Returns the type expression for the given variable or 'Nothing' if no such
--   type expression exists.
lookupVarType :: Int -> TIMonad a (Maybe (TypeExpr a))
lookupVarType v = do (_, _, _, vsub) <- get
                     return (DM.lookup v vsub)

-- | Inserts the given type expression for the given variable into the type
--   inference state.
insertVarType :: Int -> TypeExpr a -> TIMonad a ()
insertVarType v te
  = modify (\(tenv, n, tsenv, vsub) -> (tenv, n, tsenv, DM.insert v te vsub))

-- | Inserts the given type expression for the given qualified name into the
--   type signature environment of the type inference state.
insertFunType :: QName -> TypeExpr a -> TIMonad a ()
insertFunType qn te
  = do te' <- freshVariant te
       modify (\(x, n, tsenv, y) -> (x, n, insertType qn te' tsenv, y))

-- | Extends the type environment with the given list of mappings from qualified
--   names to type expressions in the type inference state.
extendTypeEnv :: [(QName, TypeExpr a)] -> TIMonad a ()
extendTypeEnv ms
  = modify (\(tenv, n, x, y) -> (DM.union (listToTypeEnv ms) tenv, n, x, y))

-- | Returns a fresh variant (with renamed type variables) of the given type
--   expression.
freshVariant :: TypeExpr a -> TIMonad a (TypeExpr a)
freshVariant te = snd <$> rename DM.empty te
  where
    rename sub (TVar ((v, _), x))
      = case DM.lookup v sub of
          Just v' -> return (sub, v')
          Nothing -> do v' <- nextTVar x
                        return (DM.insert v v' sub, v')
    rename sub (FuncType x t1 t2) = do (sub', t1') <- rename sub t1
                                       (sub'', t2') <- rename sub' t2
                                       return (sub'', FuncType x t1' t2')
    rename sub (TCons x qn tes)   = do (sub', tes') <- mapAccumM rename sub tes
                                       return (sub', TCons x qn tes')

-- | Returns a type variant for the given qualified name.
getTypeVariant :: QName -> TIMonad a (TypeExpr a)
getTypeVariant qn = do (tenv, _, tsenv, _) <- get
                       case lookupType qn tenv of
                         Nothing -> case lookupType qn tsenv of
                                      Nothing -> throwError err
                                      Just te -> return te
                         Just te -> do te' <- freshVariant te
                                       return te'
  where
    err = TIError ("There is no type expression for "
                     ++ showQName defaultAHOptions qn
                     ++ "!")

-- -----------------------------------------------------------------------------
-- Functions for type annotation of abstract Haskell programs
-- -----------------------------------------------------------------------------

-- | Annotates the given program with fresh type variables.
annProg :: Prog a -> TIMonad a (Prog a)
annProg (Prog mn is tds fds) = do fds' <- mapM annFunc fds
                                  return (Prog mn is tds fds')

-- | Annotates the given function declaration with fresh type variables.
annFunc :: FuncDecl a -> TIMonad a (FuncDecl a)
annFunc (Func x y@(qn, _) a v _ rs) = do initVarTypes
                                         te <- getTypeVariant qn
                                         rs' <- annRules rs
                                         return (Func x y a v (TypeSig te) rs')

-- | Annotates the given rules declaration with fresh type variables.
annRules :: Rules a -> TIMonad a (Rules a)
annRules (Rules rs)     = do rs' <- mapM annRule rs
                             return (Rules rs')
annRules (External x _) = do te <- nextTVar x
                             return (External x (TypeAnn te))

-- | Annotates the given function rule with fresh type variables.
annRule :: Rule a -> TIMonad a (Rule a)
annRule (Rule x _ ps rhs lds) = do te <- nextTVar x
                                   ps' <- mapM annPattern ps
                                   rhs' <- annRhs rhs
                                   lds' <- mapM annLocalDecl lds
                                   return (Rule x (TypeAnn te) ps' rhs' lds')

-- | Annotates the given right-hand side with fresh type variables.
annRhs :: Rhs a -> TIMonad a (Rhs a)
annRhs (SimpleRhs e)      = do e' <- annExpr e
                               return (SimpleRhs e')
annRhs (GuardedRhs x eqs) = do eqs' <- mapM (bothM annExpr) eqs
                               return (GuardedRhs x eqs')

-- | Annotates the given local declaration with fresh type variables.
annLocalDecl :: LocalDecl a -> TIMonad a (LocalDecl a)
annLocalDecl _ = throwError (TIError "Local declarations can not be annotated!")

-- | Annotates the given statement with fresh type variables.
annStatement :: Statement a -> TIMonad a (Statement a)
annStatement (SExpr e)    = do e' <- annExpr e
                               return (SExpr e')
annStatement (SPat x p e) = do p' <- annPattern p
                               e' <- annExpr e
                               return (SPat x p' e')
annStatement (SLet x lds) = do lds' <- mapM annLocalDecl lds
                               return (SLet x lds')

-- | Annotates the given pattern with fresh type variables.
annPattern :: Pattern a -> TIMonad a (Pattern a)
annPattern (PVar _ y@((v, _), x))      = do te <- nextTVar x
                                            insertVarType v te
                                            return (PVar (TypeAnn te) y)
annPattern (PLit _ l@(_, x))           = do te <- nextTVar x
                                            return (PLit (TypeAnn te) l)
annPattern (PComb x _ y@(qn, _) ps)    = do te <- getTypeVariant qn
                                            ps' <- mapM annPattern ps
                                            return (PComb x (TypeAnn te) y ps')
annPattern (PAs x _ vn@((v, _), vx) p) = do te <- nextTVar vx
                                            insertVarType v te
                                            p' <- annPattern p
                                            return (PAs x (TypeAnn te) vn p')
annPattern (PTuple x _ ps)             = do te <- nextTVar x
                                            ps' <- mapM annPattern ps
                                            return (PTuple x (TypeAnn te) ps')
annPattern (PList x _ ps)              = do te <- nextTVar x
                                            ps' <- mapM annPattern ps
                                            return (PList x (TypeAnn te) ps')

-- | Annotates the given branch expression with fresh type variables.
annBranchExpr :: BranchExpr a -> TIMonad a (BranchExpr a)
annBranchExpr (Branch x p e) = Branch x <$> annPattern p <*> annExpr e

-- | Annotates the given expression with fresh type variables.
annExpr :: Expr a -> TIMonad a (Expr a)
annExpr (Var _ x@(vn@(v, _), _))
  = lookupVarType v
      >>= maybe (throwError err) (\te -> return (Var (TypeAnn te) x))
  where
    err = TIError ("There is no type variable for " ++ showVarName vn ++ "!")
annExpr (Lit _ l@(_, x))          = do te <- nextTVar x
                                       return (Lit (TypeAnn te) l)
annExpr (Symbol _ x@(qn, _))      = do te <- getTypeVariant qn
                                       return (Symbol (TypeAnn te) x)
annExpr (Apply x _ e1 e2)         = do te <- nextTVar x
                                       e1' <- annExpr e1
                                       e2' <- annExpr e2
                                       return (Apply x (TypeAnn te) e1' e2')
annExpr (InfixApply x _ e1 qn e2)
  = do te <- getTypeVariant (fst qn)
       e1' <- annExpr e1
       e2' <- annExpr e2
       return (InfixApply x (TypeAnn te) e1' qn e2')
annExpr (Lambda x _ ps e)         = do te <- nextTVar x
                                       ps' <- mapM annPattern ps
                                       e' <- annExpr e
                                       return (Lambda x (TypeAnn te) ps' e')
annExpr (Let x _ lds e)           = do te <- nextTVar x
                                       lds' <- mapM annLocalDecl lds
                                       e' <- annExpr e
                                       return (Let x (TypeAnn te) lds' e')
annExpr (DoExpr x _ sts)          = do te <- nextTVar x
                                       sts' <- mapM annStatement sts
                                       return (DoExpr x (TypeAnn te) sts')
annExpr (ListComp x _ e sts)      = do te <- nextTVar x
                                       e' <- annExpr e
                                       sts' <- mapM annStatement sts
                                       return (ListComp x (TypeAnn te) e' sts')
annExpr (Case x _ e bes)          = do te <- nextTVar x
                                       e' <- annExpr e
                                       bes' <- mapM annBranchExpr bes
                                       return (Case x (TypeAnn te) e' bes')
annExpr (Typed x ta e te)         = do tae <- nextTVar x
                                       e' <- annExpr e
                                       te' <- freshVariant te
                                       return (Typed x (TypeAnn tae) e' te')
annExpr (IfThenElse x _ e1 e2 e3)
  = do te <- nextTVar x
       e1' <- annExpr e1
       e2' <- annExpr e2
       e3' <- annExpr e3
       return (IfThenElse x (TypeAnn te) e1' e2' e3')
annExpr (Tuple x _ es)            = do te <- nextTVar x
                                       es' <- mapM annExpr es
                                       return (Tuple x (TypeAnn te) es')
annExpr (List x _ es)             = do te <- nextTVar x
                                       es' <- mapM annExpr es
                                       return (List x (TypeAnn te) es')

-- -----------------------------------------------------------------------------
-- Functions for creating type expression equations
-- -----------------------------------------------------------------------------

-- | Returns the type expression equations for the given rules declaration and
--   the given function type expression.
eqsRules :: TypeExpr a -> Rules a -> TIMonad a (TypeExprEqs a)
eqsRules te (Rules rs)                 = concat <$> mapM (eqsRule te) rs
eqsRules te (External _ (TypeAnn tae)) = return [te =.= tae]
eqsRules _  (External _ NoTypeAnn)     = throwError err
  where
    err = TIError "External declaration is not annotated with a type variable!"

-- | Returns the type expression equations for the given function rule and the
--   given function type expression.
eqsRule :: TypeExpr a -> Rule a -> TIMonad a (TypeExprEqs a)
eqsRule te (Rule x (TypeAnn tae) ps rhs _)
  = let rhstes = catMaybes (rhsType rhs)
        ptes = catMaybes (map patternType ps)
        eqs = map (\ty -> foldr1 (FuncType x) (ptes ++ [ty])) rhstes
     in return ([te =.= tae] ++ map (tae =.=) eqs)
          ++= (concat <$> mapM (uncurry eqsPattern) (zip ptes ps))
          ++= eqsRhs rhs

-- | Returns a type expression equation for the given guard expression.
eqsGuard :: Expr a -> TIMonad a (TypeExprEq a)
eqsGuard e = let x = exprAnn e
              in return (boolType x x =.= fromJust (exprType e))

-- | Returns the type expression equations for the given right-hand side.
eqsRhs :: Rhs a -> TIMonad a (TypeExprEqs a)
eqsRhs (SimpleRhs e)      = eqsExpr e
eqsRhs (GuardedRhs _ eqs) = do eqs' <- concat <$> mapM (eqsExpr . snd) eqs
                               geqs <- mapM (eqsGuard . fst) eqs
                               return (eqs' ++ geqs)

-- | Returns the type expression equations for the given branch expression and
--   the given case type expression and case expression type expression.
eqsBranch :: TypeExpr a -> TypeExpr a -> BranchExpr a
          -> TIMonad a (TypeExprEqs a)
eqsBranch te' te (Branch _ p e) = return [te' =.= fromJust (exprType e)]
                                    ++= eqsPattern te p
                                    ++= eqsExpr e

-- | Returns the type expression equations for the given pattern with the given
--   type expression.
eqsPattern :: TypeExpr a -> Pattern a -> TIMonad a (TypeExprEqs a)
eqsPattern te (PVar (TypeAnn tae) _)       = return [te =.= tae]
eqsPattern te (PLit (TypeAnn tae) (l, x))
  = return [te =.= tae, tae =.= literalType l x x]
--------------------------------------------------------------------------------
eqsPattern te (PComb _ (TypeAnn tae) _ ps)
  = return [te =.= tae] ++= eqsC tae ps
  where
    eqsC :: TypeExpr a -> [Pattern a] -> TIMonad a (TypeExprEqs a)
    eqsC _ []      = return []
    eqsC te (p:ps) = return [leftFuncType te =.= fromJust (patternType p)]
                       ++= eqsC (rightFuncType te) ps
eqsPattern te (PAs _ (TypeAnn tae) _ p)
  = return [te =.= tae, tae =.= fromJust (patternType p)]
eqsPattern te (PTuple x (TypeAnn tae) ps)
  = return [te =.= tae, tae =.= tupleType (catMaybes (map patternType ps)) x x]
eqsPattern te (PList x (TypeAnn tae) ps)
  = return [te =.= tae, tae =.= listType (fromJust (patternType (head ps))) x x]
      ++= eqsP (fromJust (patternType (head ps))) (tail ps)
  where
    eqsP :: TypeExpr a -> [Pattern a] -> TIMonad a (TypeExprEqs a)
    eqsP te []     = return []
    eqsP te (p:ps) = return [te =.= fromJust (patternType p)]
                       ++= eqsP te ps

eqsExpr :: Expr a -> TIMonad a (TypeExprEqs a)
eqsExpr (Var _ x@(vn, _))                    = return []
eqsExpr (Lit (TypeAnn ty) l) = return [ty =.= literalType (fst l) (snd l) (snd l)] 
eqsExpr (Symbol (TypeAnn ty) qn)             = return []
eqsExpr (Apply _ (TypeAnn ty) e1 e2)
  = return [leftFuncType (fromJust (exprType e1)) =.= fromJust (exprType e2),
            rightFuncType (fromJust (exprType e1)) =.= ty]
      ++= eqsExpr e1 ++= eqsExpr e2
eqsExpr (InfixApply x ta e1 qn e2)           = undefined
eqsExpr (Lambda _ (TypeAnn ty) pats e)       = undefined
eqsExpr (Let _ (TypeAnn ty) ls e)            = undefined
eqsExpr (DoExpr _ (TypeAnn ty) sts)          = undefined
eqsExpr (ListComp _ (TypeAnn ty) e sts)      = undefined
eqsExpr (Case _ (TypeAnn ty) e bs)
  = eqsExpr e ++= (concat <$> mapM (eqsBranch ty (fromJust (exprType e))) bs)
eqsExpr (Typed _ (TypeAnn ty) e te)
  = return [ty =.= fromJust (exprType e), ty =.= te] ++= eqsExpr e
eqsExpr (IfThenElse _ (TypeAnn ty) e1 e2 e3)
  = return [fromJust (exprType e1) =.= boolType (exprAnn e1) (exprAnn e1),
            ty =.= fromJust (exprType e2), ty =.= fromJust (exprType e3)]
      ++= eqsExpr e1 ++= eqsExpr e2 ++= eqsExpr e3
eqsExpr (Tuple _ (TypeAnn ty) es)            = concat <$> mapM eqsExpr es
eqsExpr (List _ (TypeAnn ty) es)             = concat <$> mapM eqsExpr es

-- TODO: Add Prelude type environment.
getTypeEnv :: Prog a -> TypeEnv a
getTypeEnv p = extractKnownTypes [p]

extractKnownTypes :: [Prog a] -> TypeEnv a
extractKnownTypes = listToTypeEnv . (concatMap extractProg)
  where
    extractProg :: Prog a -> [(QName, TypeExpr a)]
    extractProg (Prog _ _ td fd) = concatMap extractTypeDecl td
                                     ++ catMaybes (map extractFuncDecl fd)

    extractFuncDecl :: FuncDecl a -> Maybe (QName, TypeExpr a)
    extractFuncDecl (Func _ (n, _) _ _ ty _) = do te <- typeSigType ty
                                                  return (n, te)

    extractTypeDecl :: TypeDecl a -> [(QName, TypeExpr a)]
    extractTypeDecl (TypeSyn _ (n, _) _ _ ty) = [(n, ty)]
    extractTypeDecl (Type x qn _ vs cs)
      = map (extractConsDecl (TCons x qn (map TVar vs))) cs

    extractConsDecl :: TypeExpr a -> ConsDecl a -> (QName, TypeExpr a)
    extractConsDecl ty (Cons x (n, _) _ _ tys) = (n, foldr (FuncType x) ty tys)

inferFuncDecl :: Prog a -> QName -> Either (TIError a) (FuncDecl a)
inferFuncDecl p = inferFunctionEnv (getTypeEnv p) p

inferHSE :: Module a -> Either (TIError b) (Prog b)
inferHSE = undefined

inferProg :: Show a => Prog a -> Either (TIError a) (Prog a)
inferProg p
  = case inferProgEnv (getTypeEnv pnew) pnew of
       Left e -> Left e
       Right p' -> let newsf = case inferNewFunctions p (getFuncsWTS p) of
                                 Left e   -> error (show e)
                                 Right xs -> xs
                    in Right (addFWTS p' newsf)
  where
    pnew = getFuncsWithTS p

getFuncsWTS :: Prog a -> [FuncDecl a]
getFuncsWTS (Prog _ _ _ fd) = filter (not . hasTypeSig) fd

getFuncsWithTS :: Prog a -> Prog a
getFuncsWithTS (Prog a b c fd) = Prog a b c (filter hasTypeSig fd)

addFWTS :: Prog a -> [FuncDecl a] -> Prog a
addFWTS (Prog a b c fd) fds = Prog a b c (fd ++ fds)

replaceFWTS :: Prog a -> [FuncDecl a] -> Prog a
replaceFWTS p@(Prog a b c fds) newsf = Prog a b c [change fd | fd <- fds]
  where
    change fd = case find ((== funcName fd) . funcName) newsf of
                  Nothing -> fd
                  Just f  -> f

inferNewFunctions :: Prog a -> [FuncDecl a] -> Either (TIError a) [FuncDecl a]
inferNewFunctions p@(Prog (mn, _) _ _ _)
  = inferNewFunctionsEnv (getTypeEnv p) mn

inferExpr :: Prog a -> Expr a -> Either (TIError a) (Expr a)
inferExpr p = inferExprEnv (getTypeEnv p)

inferProgEnv :: TypeEnv a -> Prog a -> Either (TIError a) (Prog a)
inferProgEnv te p
  = evalState (runExceptT (annProg p >>= inferAProg)) (initTIState te)

inferFunctionEnv :: TypeEnv a -> Prog a -> QName
                 -> Either (TIError a) (FuncDecl a)
inferFunctionEnv te (Prog _ _ _ fs) qn
  = case find ((== qn) . funcName) fs of
      Nothing -> Left (TIError ("No such function: " ++ snd qn))
      Just fd -> evalState (runExceptT (annFunc fd >>= inferFunc))
                           (initTIState te)

inferNewFunctionsEnv :: TypeEnv a -> MName -> [FuncDecl a]
                     -> Either (TIError a) [FuncDecl a]
inferNewFunctionsEnv te mid fs
  = evalState (runExceptT (infer (depGraph mid fs))) (initTIState te)
  where
    infer fss     = do fs' <- concat <$> mapM inferGroup fss
                       mapM (flip extract fs') fs
    inferGroup g
      = annFuncGroup g >>= inferFuncGroup >>= \afs ->
        extendTypeEnv [(qn, ty) | Func _ (qn, _) _ _ (TypeSig ty) _ <- afs] >>
        return afs
    extract :: FuncDecl a -> [FuncDecl a] -> TIMonad a (FuncDecl a)
    extract f afs = case find ((== funcName f) . funcName) afs of
                      Just af -> return af
                      Nothing -> throwError (TIError "Internal error: extract")

inferExprEnv :: TypeEnv a -> Expr a -> Either (TIError a) (Expr a)
inferExprEnv te e
  = evalState (runExceptT (annExpr e >>= inferAExpr)) (initTIState te)

annFuncGroup :: [FuncDecl a] -> TIMonad a [FuncDecl a]
annFuncGroup fs = initSigEnv >> mapM (uncurry insertFunType) ftys >>
                  mapM annFunc fs
  where ftys = [ (qn, ty) | Func _ (qn, _) _ _ (TypeSig ty) _ <- fs]

inferAProg :: Prog a -> TIMonad a (Prog a)
inferAProg (Prog mid is td fd)
  = (\fd' -> Prog mid is td fd') <$> mapM inferFunc fd

inferFunc :: FuncDecl a -> TIMonad a (FuncDecl a)
inferFunc func@(Func _ _ _ _ (TypeSig ty) rs) =
  eqsRules ty rs >>= \ eqs    ->
  solve eqs >>= \ sigma ->
  return $ normalize normFuncDecl (applyTESubstFD sigma func)

inferFuncGroup :: [FuncDecl a] -> TIMonad a [FuncDecl a]
inferFuncGroup fs =
  (concat <$> mapM (uncurry eqsRules)
                   [(ty, rs) | Func _ _ _ _ (TypeSig ty) rs <- fs])
  >>= \eqs ->
  solve eqs >>= \ sigma ->
  mapM (return . normalize normFuncDecl . applyTESubstFD sigma) fs >>= \afs ->
  return afs

inferAExpr :: Expr a -> TIMonad a (Expr a)
inferAExpr e = eqsExpr e >>= \eqs   ->
               solve eqs >>= \sigma ->
               return $ normalize normExpr (applyTESubstE sigma e)