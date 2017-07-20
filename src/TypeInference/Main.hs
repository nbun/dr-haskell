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
import Goodies ((++=), both, bothM, concatMapM, mapAccumM, one, two)
import Language.Haskell.Exts (Module)
import TypeInference.AbstractHaskell
import TypeInference.AbstractHaskellGoodies
import TypeInference.HSE2AH (hseToAH)
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

-- | Returns the type environment for the given program.
getTypeEnv :: Prog a -> TypeEnv a
getTypeEnv p = extractKnownTypes [p]

-- | Returns the type environment extracted from the given list of programs.
extractKnownTypes :: [Prog a] -> TypeEnv a
extractKnownTypes = listToTypeEnv . (concatMap extractProg)
  where
    extractProg :: Prog a -> [(QName, TypeExpr a)]
    extractProg (Prog _ _ td fd)
      = concatMap extractTypeDecl td ++ catMaybes (map extractFuncDecl fd)

    extractFuncDecl :: FuncDecl a -> Maybe (QName, TypeExpr a)
    extractFuncDecl (Func _ (qn, _) _ _ ts _) = do te <- typeSigType ts
                                                   return (qn, te)

    extractTypeDecl :: TypeDecl a -> [(QName, TypeExpr a)]
    extractTypeDecl (TypeSyn _ (qn, _) _ _ te) = [(qn, te)]
    extractTypeDecl (Type x qn _ vs cs)
      = map (extractConsDecl (TCons x qn (map TVar vs))) cs

    extractConsDecl :: TypeExpr a -> ConsDecl a -> (QName, TypeExpr a)
    extractConsDecl te (Cons x (qn, _) _ _ tes)
      = (qn, foldr (FuncType x) te tes)

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

-- | Returns the annotated type from the given expression or 'Nothing' if no
--   type is annotated.
exprType' :: Expr a -> Maybe (TypeExpr a)
exprType' (InfixApply _ ta _ _ _) = typeAnnType ta >>= (return . returnType)
exprType' e                       = exprType e

-- | Returns the annotated type from the given pattern or 'Nothing' if no type
--   is annotated.
patternType' :: Pattern a -> Maybe (TypeExpr a)
patternType' (PComb _ ta _ _) = typeAnnType ta >>= (return . returnType)
patternType' p                = patternType p

-- | Returns the type expression equations for the given rules declaration and
--   the given function type expression.
eqsRules :: TypeExpr a -> Rules a -> TIMonad a (TypeExprEqs a)
eqsRules te (Rules rs)                 = concatMapM (eqsRule te) rs
eqsRules te (External _ (TypeAnn tae)) = return [te =.= tae]
eqsRules _  _                          = return []

-- | Returns the type expression equations for the given function rule and the
--   given function type expression.
eqsRule :: TypeExpr a -> Rule a -> TIMonad a (TypeExprEqs a)
eqsRule te (Rule x (TypeAnn tae) ps rhs _)
  = let rhstes = catMaybes (rhsType rhs)
        ptes = catMaybes (map patternType' ps)
        eqs = map (\ty -> foldr1 (FuncType x) (ptes ++ [ty])) rhstes
     in return ([te =.= tae] ++ map (tae =.=) eqs)
          ++= (concatMapM (uncurry eqsPattern) (zip ptes ps))
          ++= eqsRhs rhs
eqsRule _  _                               = return []

-- | Returns a type expression equation for the given guard expression.
eqsGuard :: Expr a -> TIMonad a (TypeExprEq a)
eqsGuard e = let x = exprAnn e
              in return (boolType x x =.= fromJust (exprType' e))

-- | Returns the type expression equations for the given right-hand side.
eqsRhs :: Rhs a -> TIMonad a (TypeExprEqs a)
eqsRhs (SimpleRhs e)      = eqsExpr e
eqsRhs (GuardedRhs _ eqs) = do eqs' <- concatMapM (eqsExpr . snd) eqs
                               geqs <- mapM (eqsGuard . fst) eqs
                               return (eqs' ++ geqs)

-- | Returns the type expression equations for the given branch expression and
--   the given case type expression and case expression type expression.
eqsBranch :: TypeExpr a -> TypeExpr a -> BranchExpr a
          -> TIMonad a (TypeExprEqs a)
eqsBranch te' te (Branch _ p e) = return [te' =.= fromJust (exprType' e)]
                                    ++= eqsPattern te p
                                    ++= eqsExpr e

-- | Returns the type expression equations for the given pattern with the given
--   type expression.
eqsPattern :: TypeExpr a -> Pattern a -> TIMonad a (TypeExprEqs a)
eqsPattern te (PVar (TypeAnn tae) _)       = return [te =.= tae]
eqsPattern te (PLit (TypeAnn tae) (l, x))
  = return [te =.= tae, tae =.= literalType l x x]
eqsPattern te (PComb x (TypeAnn tae) _ ps)
  = let ptes = catMaybes (map patternType' ps)
     in return [te =.= returnType tae,
                tae =.= foldr1 (FuncType x) (ptes ++ [returnType tae])]
          ++= concatMapM (uncurry eqsPattern) (zip ptes ps)
eqsPattern te (PAs _ (TypeAnn tae) _ p)
  = let pt = fromJust (patternType' p)
     in return [te =.= tae, tae =.= pt] ++= eqsPattern pt p
eqsPattern te (PTuple x (TypeAnn tae) ps)
  = let ptes = catMaybes (map patternType' ps)
     in return [te =.= tae, tae =.= tupleType ptes x x]
          ++= concatMapM (uncurry eqsPattern) (zip ptes ps)
eqsPattern te (PList x (TypeAnn tae) ps)
  = let ptes = catMaybes (map patternType' ps)
     in return ([te =.= tae, tae =.= listType (head ptes) x x]
                  ++ map (head ptes =.=) (tail ptes))
          ++= concatMapM (uncurry eqsPattern) (zip ptes ps)
eqsPattern _  _                            = return []

-- | Returns the type expression equations for the given expression.
eqsExpr :: Expr a -> TIMonad a (TypeExprEqs a)
eqsExpr (Lit (TypeAnn te) (l, x))            = return [te =.= literalType l x x]
eqsExpr (Apply _ (TypeAnn te) e1 e2)
  = let lte = fromJust (exprType' e1)
     in return [leftFuncType lte =.= fromJust (exprType' e2),
                te =.= rightFuncType lte]
          ++= eqsExpr e1
          ++= eqsExpr e2
eqsExpr (InfixApply _ (TypeAnn te) e1 _ e2)
  = let lte = fromJust (exprType' e1)
        rte = fromJust (exprType' e2)
     in return [leftFuncType te =.= lte,
                leftFuncType (rightFuncType te) =.= rte]
          ++= eqsExpr e1
          ++= eqsExpr e2
eqsExpr (Lambda x (TypeAnn te) ps e)
  = let ptes = catMaybes (map patternType' ps)
        te' = fromJust (exprType' e)
     in return [te =.= foldr1 (FuncType x) (ptes ++ [te'])]
          ++= (concatMapM (uncurry eqsPattern) (zip ptes ps))
          ++= eqsExpr e
eqsExpr (DoExpr _ _ _)
  = throwError (TIError "do-expressions are not supported yet!")
eqsExpr (ListComp _ _ _ _)
  = throwError (TIError "List comprehensions are not supported yet!")
eqsExpr (Case _ (TypeAnn te) e bs)
  = eqsExpr e ++= (concatMapM (eqsBranch te (fromJust (exprType' e))) bs)
eqsExpr (Typed _ (TypeAnn tae) e te)
  = return [tae =.= fromJust (exprType' e), tae =.= te] ++= eqsExpr e
eqsExpr (IfThenElse _ (TypeAnn te) e1 e2 e3)
  = let ate = fromJust (exprType' e1)
        bte = fromJust (exprType' e2)
        cte = fromJust (exprType' e3)
        ax = exprAnn e1
     in return [ate =.= boolType ax ax, te =.= bte, te =.= cte]
          ++= eqsExpr e1
          ++= eqsExpr e2
          ++= eqsExpr e3
eqsExpr (Tuple x (TypeAnn te) es)
  = let etes = catMaybes (map exprType' es)
     in return [te =.= tupleType etes x x]
          ++= concatMapM eqsExpr es
eqsExpr (List x (TypeAnn te) es)
  = let etes = catMaybes (map exprType' es)
     in return ([te =.= listType (head etes) x x]
                  ++ map (head etes =.=) (tail etes))
          ++= concatMapM eqsExpr es
eqsExpr _                                    = return []

-- -----------------------------------------------------------------------------
-- Functions for type inference of abstract Haskell programs
-- -----------------------------------------------------------------------------

-- | Infers the given function declaration with the given program. The function
--   declaration may not be included in the given program.
inferFuncDecl :: Prog a -> FuncDecl a -> Either (TIError a) (FuncDecl a)
inferFuncDecl p = inferFuncDeclEnv (getTypeEnv p)

-- | Infers the given function declaration with the given type environment.
inferFuncDeclEnv :: TypeEnv a -> FuncDecl a -> Either (TIError a) (FuncDecl a)
inferFuncDeclEnv tenv fd
  = evalState (runExceptT (addFD fd >> annFunc fd >>= inferFunc))
              (initTIState tenv)
  where
    addFD :: FuncDecl a -> TIMonad a ()
    addFD (Func x (qn, _) _ _ Untyped _)      = insertFunType qn (teVar 0 x)
    addFD (Func _ (qn, _) _ _ (TypeSig te) _) = insertFunType qn te

-- | Infers the given already annotated function declaration.
inferFunc :: FuncDecl a -> TIMonad a (FuncDecl a)
inferFunc fd@(Func _ _ _ _ (TypeSig te) rs)
  = do eqs <- eqsRules te rs
       sub <- solve eqs
       return (normalize normFuncDecl (applyTESubstFD sub fd))

-- | Annotates the given group of function declarations with fresh type
--   variables.
annFuncGroup :: [FuncDecl a] -> TIMonad a [FuncDecl a]
annFuncGroup fds = do initSigEnv
                      mapM addFD fds
                      mapM annFunc fds
  where
    addFD :: FuncDecl a -> TIMonad a ()
    addFD (Func x (qn, _) _ _ Untyped _)      = insertFunType qn (teVar 0 x)
    addFD (Func _ (qn, _) _ _ (TypeSig te) _) = insertFunType qn te

-- | Infers the given expression with the given program.
inferExpr :: Prog a -> Expr a -> Either (TIError a) (Expr a)
inferExpr p = inferExprEnv (getTypeEnv p)

-- | Infers the given expression with the given type environment.
inferExprEnv :: TypeEnv a -> Expr a -> Either (TIError a) (Expr a)
inferExprEnv tenv e
  = evalState (runExceptT (annExpr e >>= inferAExpr)) (initTIState tenv)

-- | Infers the given already annotated expression.
inferAExpr :: Expr a -> TIMonad a (Expr a)
inferAExpr e = do eqs <- eqsExpr e
                  sub <- solve eqs
                  return (normalize normExpr (applyTESubstE sub e))

-- | Infers the given program with the 'Language.Haskell.Exts.Syntax'
--   representation.
inferHSE :: Module a -> Either (TIError a) (Prog a)
inferHSE = inferProg . hseToAH

-- | Returns the list of function declarations without type signatures.
getFuncsWTS :: Prog a -> [FuncDecl a]
getFuncsWTS (Prog _ _ _ fds) = filter (not . hasTypeSig) fds

-- | Infers the given program.
inferProg :: Prog a -> Either (TIError a) (Prog a)
inferProg p = evalState (runExceptT (inferProg' p)) (initTIState (getTypeEnv p))

-- | Infers the given program.
inferProg' :: Prog a -> TIMonad a (Prog a)
inferProg' p = let fdswts = getFuncsWTS p
                in do fds <- inferNewFunctionsEnv (modName p) fdswts
                      p' <- inferProgEnv (getProgFDWTS p)
                      return (replaceFWTS p' fds)
  where
    getProgFDWTS :: Prog a -> Prog a
    getProgFDWTS (Prog a b c fd) = Prog a b c (filter hasTypeSig fd)
    replaceFWTS :: Prog a -> [FuncDecl a] -> Prog a
    replaceFWTS p@(Prog a b c fds) fds' = Prog a b c [change fd | fd <- fds]
      where
        change fd = case find ((== funcName fd) . funcName) fds' of
                      Nothing -> fd
                      Just f  -> f

inferProgEnv :: Prog a -> TIMonad a (Prog a)
inferProgEnv p
  = annProg p >>= inferAProg

inferAProg :: Prog a -> TIMonad a (Prog a)
inferAProg (Prog mid is td fd)
  = (\fd' -> Prog mid is td fd') <$> mapM inferFunc fd

inferNewFunctionsEnv :: MName -> [FuncDecl a] -> TIMonad a [FuncDecl a]
inferNewFunctionsEnv mid fs = infer (depGraph mid fs)
  where
    infer fss = do fs' <- concatMapM inferGroup fss
                   mapM (flip extract fs') fs
    inferGroup g
      = do xs <- annFuncGroup g
           afs <- inferFuncGroup xs
           extendTypeEnv [(qn, ty) | Func _ (qn, _) _ _ (TypeSig ty) _ <- afs]
           return afs
    extract :: FuncDecl a -> [FuncDecl a] -> TIMonad a (FuncDecl a)
    extract f afs = case find ((== funcName f) . funcName) afs of
                      Just af -> return af
                      Nothing -> throwError (TIError "Internal error: extract")

inferFuncGroup :: [FuncDecl a] -> TIMonad a [FuncDecl a]
inferFuncGroup fs
  = do eqs <- concatMapM (uncurry eqsRules)
                         [(ty, rs) | Func _ _ _ _ (TypeSig ty) rs <- fs]
       sub <- solve eqs
       afs <- mapM (return . normalize normFuncDecl . applyTESubstFD sub) fs
       return afs