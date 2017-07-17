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
import Goodies ((++=), both, bothM, mapAccumM)
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

-- | A type environment represented as a map from variables to type expressions
--   and parameterized over the type of annotations.
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
fromTypeExpr :: TypeExpr a -> Term QName a
fromTypeExpr (TVar ((v, _), x))    = TermVar x v
fromTypeExpr (FuncType x t1 t2)
  = TermCons x (preName "->") [fromTypeExpr t1, fromTypeExpr t2]
fromTypeExpr (TCons x (qn, _) tes) = TermCons x qn (map fromTypeExpr tes)

-- | Converts the given term representation into a type expression.
toTypeExpr :: Term QName a -> TypeExpr a
toTypeExpr (TermVar x v)      = teVar v x
toTypeExpr (TermCons x qn ts)
  | snd qn == "->" = FuncType x (toTypeExpr (ts !! 0)) (toTypeExpr (ts !! 1))
  | otherwise      = TCons x (qn, x) (map toTypeExpr ts)

-- | Converts the given list of type expression equations into a list of term
--   equations.
fromTypeExprEqs :: TypeExprEqs a -> TermEqs QName a
fromTypeExprEqs = map (both fromTypeExpr)

-- | Converts the given list of term equations into a list of type expression
--   equations.
toTypeExprEqs :: TermEqs QName a -> TypeExprEqs a
toTypeExprEqs = map (both toTypeExpr)

-- | Solves the given list of type expression equations.
solve :: TypeExprEqs a -> TIMonad a (TESubst a)
solve eqs = case unify (fromTypeExprEqs eqs) of
              Left e    -> throwError (toTIError e)
              Right sub -> return (DM.map toTypeExpr sub)
  where
    toTIError :: UnificationError QName a -> TIError a
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
--   type inference state.
insertFunType :: QName -> TypeExpr a -> TIMonad a ()
insertFunType qn te
  = do t <- freshVariant te
       modify (\(tenv, n, tsenv, vsub) -> (tenv, n, DM.insert qn t tsenv, vsub))

-- | Extends the type signature environment with the given list of mappings from
--   qualified names to type expressions in the type inference state.
extendTypeEnv :: [(QName, TypeExpr a)] -> TIMonad a ()
extendTypeEnv ftes
  = modify (\(tenv, a, b, c) -> (DM.union tenv (DM.fromList ftes), a, b, c))

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
getTypeVariant :: QName -> TIMonad a (QName, TypeExpr a)
getTypeVariant qn = do (tenv, _, tsenv, _) <- get
                       case lookupType qn tenv of
                         Nothing -> case DM.lookup qn tsenv of
                                      Nothing -> throwError err
                                      Just te -> return (qn, te)
                         Just te -> do te' <- freshVariant te
                                       return (qn, te')
  where
    err = TIError ("There is no type expression for "
                     ++ showQName defaultAHOptions qn
                     ++ "!")

-- -----------------------------------------------------------------------------
-- Functions for type annotation of abstract haskell programs
-- -----------------------------------------------------------------------------

-- | Annotates the given program with fresh type variables.
annProg :: Prog a -> TIMonad a (Prog a)
annProg (Prog mn is tds fds) = do fds' <- mapM annFunc fds
                                  return (Prog mn is tds fds')

-- | Annotates the given function declaration with fresh type variables.
annFunc :: FuncDecl a -> TIMonad a (FuncDecl a)
annFunc (Func x y@(qn, _) a v _ rs) = do initVarTypes
                                         (_, te) <- getTypeVariant qn
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

-- | Annotates the given rules right-hand side with fresh type variables.
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
                                            tae <- nextTVar x
                                            return (PVar (TypeAnn tae) y)
annPattern (PLit _ l@(_, x))           = do te <- nextTVar x
                                            return (PLit (TypeAnn te) l)
annPattern (PComb x _ qn ps)           = do te <- nextTVar x
                                            ps' <- mapM annPattern ps
                                            return (PComb x (TypeAnn te) qn ps')
annPattern (PAs x _ vn@((v, _), vx) p) = do te <- nextTVar vx
                                            insertVarType v te
                                            tae <- nextTVar x
                                            p' <- annPattern p
                                            return (PAs x (TypeAnn tae) vn p')
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
annExpr (Symbol _ x@(qn, _))      = do (_, te) <- getTypeVariant qn
                                       return (Symbol (TypeAnn te) x)
annExpr (Apply x _ e1 e2)         = do te <- nextTVar x
                                       e1' <- annExpr e1
                                       e2' <- annExpr e2
                                       return (Apply x (TypeAnn te) e1' e2')
annExpr (InfixApply x _ e1 qn e2)
  = do (_, te) <- getTypeVariant (fst qn)
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
annExpr (Typed x ta e te)         = do tea <- nextTVar x
                                       e' <- annExpr e
                                       te' <- freshVariant te
                                       return (Typed x (TypeAnn tea) e' te')
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

-- | Returns the type expression equations for the given rules declaration.
eqsRules :: TypeExpr a -> Rules a -> TIMonad a (TypeExprEqs a)
eqsRules te (Rules rs)                 = concat <$> mapM (eqsRule te) rs
eqsRules te (External x (TypeAnn te')) = return [te =.= te']
eqsRules te (External x NoTypeAnn)     = throwError err
  where
    err = TIError "External declaration is not annotated with a type variable!"

-- | Returns the type expression equations for the given rules right-hand side.
eqsRhs :: Rhs a -> TIMonad a (TypeExprEqs a)
eqsRhs (SimpleRhs e)      = eqsExpr e
eqsRhs (GuardedRhs x eqs) = do eqs' <- mapM (eqsExpr . snd) eqs
                               gs <- mapM (guardEq . fst) eqs
                               return (concat eqs' ++ gs)

guardEq :: Expr a -> TIMonad a (TypeExprEq a)
guardEq e = return ((boolType (exprAnn e)) =.= (exprType' e))

-- | Returns the type expression equations for the given branch expression.
eqsBranch :: TypeExpr a -> Expr a -> BranchExpr a -> TIMonad a (TypeExprEqs a)
eqsBranch te e (Branch x p e')
  = return [te =.= exprType' e'] ++= eqsPattern (exprType' e) p ++= eqsExpr e'

exprType' :: Expr a -> TypeExpr a
exprType' = fromJust . exprType

getFirstFT :: TypeExpr a -> TypeExpr a
getFirstFT (FuncType _ te _) = te

getSecondFT :: TypeExpr a -> TypeExpr a
getSecondFT (FuncType _ _ te) = te

eqsPattern :: TypeExpr a -> Pattern a -> TIMonad a (TypeExprEqs a)
eqsPattern ty (PVar (TypeAnn te) x@(vn, _))
  = return [ty =.= te]
eqsPattern ty (PLit (TypeAnn te) l)
  = return [ty =.= te, te =.= literalType (fst l) (snd l)]
eqsPattern ty (PComb x (TypeAnn te) a@(qn, _) pats) = undefined
eqsPattern ty (PAs x (TypeAnn te) vn p)             = undefined
eqsPattern ty (PTuple x (TypeAnn te) pats)
  = return [ty =.= tupleType (catMaybes (map patternType pats)) x]
eqsPattern ty (PList x (TypeAnn te) pats)
  = undefined

eqsRule :: TypeExpr a -> Rule a -> TIMonad a (TypeExprEqs a)
eqsRule ty (Rule x (TypeAnn ty2) pats rhs _)
  = do let rhsts = catMaybes (rhsType rhs)
           pts = catMaybes (map patternType pats)
           eqs = map (\te -> foldr1 (FuncType x) (pts ++ [te])) rhsts
       return ([ty =.= ty2] ++ map (ty2 =.=) eqs) ++= eqsRhs rhs

eqsExpr :: Expr a -> TIMonad a (TypeExprEqs a)
eqsExpr (Var _ x@(vn, _))                    = return []
eqsExpr (Lit (TypeAnn ty) l) = return [ty =.= literalType (fst l) (snd l)] 
eqsExpr (Symbol (TypeAnn ty) qn)             = return []
eqsExpr (Apply _ (TypeAnn ty) e1 e2)
  = return [getFirstFT (exprType' e1) =.= exprType' e2,
            getSecondFT (exprType' e1) =.= ty]
      ++= eqsExpr e1 ++= eqsExpr e2
eqsExpr (InfixApply x ta e1 qn e2)           = undefined
eqsExpr (Lambda _ (TypeAnn ty) pats e)       = undefined
eqsExpr (Let _ (TypeAnn ty) ls e)            = undefined
eqsExpr (DoExpr _ (TypeAnn ty) sts)          = undefined
eqsExpr (ListComp _ (TypeAnn ty) e sts)      = undefined
eqsExpr (Case _ (TypeAnn ty) e bs)
  = eqsExpr e ++= (concat <$> mapM (eqsBranch ty e) bs)
eqsExpr (Typed _ (TypeAnn ty) e te)
  = return [ty =.= exprType' e, ty =.= te] ++= eqsExpr e
eqsExpr (IfThenElse _ (TypeAnn ty) e1 e2 e3)
  = return [exprType' e1 =.= boolType (exprAnn e1),
            ty =.= exprType' e2, ty =.= exprType' e3]
      ++= eqsExpr e1 ++= eqsExpr e2 ++= eqsExpr e3
eqsExpr (Tuple _ (TypeAnn ty) es)            = concat <$> mapM eqsExpr es
eqsExpr (List _ (TypeAnn ty) es)             = concat <$> mapM eqsExpr es

-- TODO: Add Prelude type environment.
getTypeEnv :: Prog a -> TypeEnv a
getTypeEnv p = extractKnownTypes [p]

extractKnownTypes :: [Prog a] -> TypeEnv a
extractKnownTypes ps = DM.fromList (concatMap extractProg ps)
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

inferHSE :: Module a -> Either (TIError a) (Prog a)
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