{-|
  This is the main library for type inference of abstract Haskell programs. It
  can also be used to infer Haskell programs with the
  'Language.Haskell.Exts.Syntax' representation.
-}

module TypeInference.Main
  ( TypeEnv, TIError (..)
  , emptyTypeEnv, lookupType, insertType, listToTypeEnv, typeEnvToList
  , composeTypeEnv, getTypeEnv, prelude, showTIError, inferExpr, inferFuncDecl
  , inferHSE, inferProg
  ) where

import           Control.Applicative                  ((<|>))
import           Control.Monad.Except                 (ExceptT, runExceptT,
                                                       throwError)
import           Control.Monad.State                  (State, evalState, get,
                                                       modify, put)
import qualified Data.Map                             as DM
import           Data.Maybe                           (catMaybes, fromJust,
                                                       mapMaybe)
import           Goodies                              (both, bothM, concatMapM,
                                                       mapAccumM, one, two,
                                                       (++=))
import           Language.Haskell.Exts                (Module, ParseResult (..),
                                                       SrcSpan (..),
                                                       SrcSpanInfo, noInfoSpan,
                                                       parseFile)
import           TypeInference.AbstractHaskell
import           TypeInference.AbstractHaskellGoodies
import           TypeInference.HSE2AH                 (hseToAH, preludeToAH)
import           TypeInference.Normalization          (normExpr, normFuncDecl,
                                                       normTypeExpr, normalize)
import           TypeInference.Term                   (Term (..), TermEqs)
import           TypeInference.TypeSubstitution       (TESubst, applyTESubstE,
                                                       applyTESubstFD)
import           TypeInference.Unification            (UnificationError (..),
                                                       unify)

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

-- | Returns a list with all mappings from the given type environment.
typeEnvToList :: TypeEnv a -> [(QName, TypeExpr a)]
typeEnvToList = DM.toList

-- | Composes the first type environment with the second type environment.
--   Mappings in the first type environment shadow those in the second.
composeTypeEnv :: TypeEnv a -> TypeEnv a -> TypeEnv a
composeTypeEnv = DM.union

-- | Returns the type environment extracted from the given list of programs.
getTypeEnv :: [Prog a] -> TypeEnv a
getTypeEnv = listToTypeEnv . concatMap extractProg
  where
    extractProg :: Prog a -> [(QName, TypeExpr a)]
    extractProg (Prog _ _ tds fds)
      = concatMap extractTypeDecl tds ++ mapMaybe extractFuncDecl fds

    extractFuncDecl :: FuncDecl a -> Maybe (QName, TypeExpr a)
    extractFuncDecl (Func _ (qn, _) _ _ ts _) = do te <- typeSigType ts
                                                   return (qn, te)

    extractTypeDecl :: TypeDecl a -> [(QName, TypeExpr a)]
    extractTypeDecl (Type x qn _ vs cs)
      = map (extractConsDecl (TCons x qn (map TVar vs))) cs
    extractTypeDecl (TypeSyn _ (qn, _) _ _ te) = [(qn, te)]

    extractConsDecl :: TypeExpr a -> ConsDecl a -> (QName, TypeExpr a)
    extractConsDecl te (Cons x (qn, _) _ _ tes)
      = (qn, foldr (FuncType x) te tes)

-- | Reads in the 'Prelude' from the given file and returns the corresponding
--   abstract Haskell representation. Adds functions for the list constructor
--   '(:)' and the tuple type constructors with a maximum arity of fifteen.
prelude :: FilePath -> IO (Prog SrcSpanInfo)
prelude fp = do (ParseOk m) <- parseFile fp
                let (Prog (_, y) _ tds fds) = preludeToAH m
                let fds' = lc : map preQualFD fds ++ tupleCons 15
                let tds' = map preQualTD tds
                return (Prog (pre, y) [] tds' fds')
  where
    a = teVar 0 x
    x = noInfoSpan (SrcSpan pre (-1) (-1) (-1) (-1))
    lc = Func x (preName "(:)", x) 2 Public (TypeSig lte) (External x NoTypeAnn)
    lte = FuncType x a (FuncType x (listType a x x) (listType a x x))

    tupleCons :: Int -> [FuncDecl SrcSpanInfo]
    tupleCons n | n < 2     = error err
                | otherwise = map tupleCons' [2..n]
      where
        err = "There is no tuple type constructor with an arity lower than two!"

    tupleCons' :: Int -> FuncDecl SrcSpanInfo
    tupleCons' n
      = let vs = map (`teVar` x) [0..n - 1]
            te = foldr (FuncType x) (tupleType vs x x) vs
         in Func x (tupleName n, x) n Public (TypeSig te) (External x NoTypeAnn)

-- | Adds the 'Prelude' qualifier to all names in the given function
--   declaration.
preQualFD :: FuncDecl a -> FuncDecl a
preQualFD (Func x ((_, n), y) a v ts _)
  = Func x (preName n, y) a v (preQualTS ts) (External x NoTypeAnn)

-- | Adds the 'Prelude' qualifier to all names in the given type signature.
preQualTS :: TypeSig a -> TypeSig a
preQualTS Untyped      = Untyped
preQualTS (TypeSig te) = TypeSig (preQualTE te)

-- | Adds the 'Prelude' qualifier to all names in the given type expression.
preQualTE :: TypeExpr a -> TypeExpr a
preQualTE (FuncType x t1 t2)        = FuncType x (preQualTE t1) (preQualTE t2)
preQualTE (TCons x ((_, n), y) tes) = TCons x (preName n, y) (map preQualTE tes)
preQualTE te                        = te

-- | Adds the 'Prelude' qualifier to all names in the given type declaration.
preQualTD :: TypeDecl a -> TypeDecl a
preQualTD (Type x ((_, n), y) v vs cds)
  = Type x (preName n, y) v vs (map preQualCD cds)
preQualTD (TypeSyn x ((_, n), y) v vs te)
  = TypeSyn x (preName n, y) v vs (preQualTE te)

-- | Adds the 'Prelude' qualifier to all names in the given constructor
--   declaration.
preQualCD :: ConsDecl a -> ConsDecl a
preQualCD (Cons x ((_, n), y) a v tes)
  = Cons x (preName n, y) a v (map preQualTE tes)

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

-- | Transforms a type inference error into a string representation.
showTIError :: TIError SrcSpanInfo -> String
showTIError (TIError e)            = e
showTIError (TIClash te1 te2)      = undefined
showTIError (TIOccurCheck vn te)
  = "OccurCheck: " ++ showVarName vn
                   ++ " occurs in "
                   ++ showTypeExpr defaultAHOptions te
                   ++ "!"
showTIError (TITooGeneral te1 te2) = undefined

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
    = FuncType x (toTypeExpr (head ts)) (toTypeExpr (ts !! 1))
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
                         Just te -> freshVariant te
  where
    err = TIError ("There is no type expression for \""
                     ++ showQName defaultAHOptions qn
                     ++ "\"!")

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

-- | Annotates the given group of function declarations with fresh type
--   variables.
annFuncGroup :: [FuncDecl a] -> TIMonad a [FuncDecl a]
annFuncGroup fds
  = do initSigEnv
       mapM_ (\fd -> insertFunType (funcName fd) (funcDeclType fd)) fds
       mapM annFunc fds

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
    err = TIError ("There is no type variable for \"" ++ showVarName vn
                                                      ++ "\"!")
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
-- Functions for creation of type expression equations
-- -----------------------------------------------------------------------------

-- | Returns the list of type expressions from the given right-hand side.
rhsTypes' :: Rhs a -> [Maybe (TypeExpr a)]
rhsTypes' (SimpleRhs e)      = [exprType' e]
rhsTypes' (GuardedRhs _ eqs) = map (exprType' . snd) eqs

-- | Returns the annotated type from the given expression or 'Nothing' if no
--   type is annotated. Returns the return type for the 'InfixApply'
--   constructor.
exprType' :: Expr a -> Maybe (TypeExpr a)
exprType' (InfixApply _ ta _ _ _) = fmap returnType (typeAnnType ta)
exprType' e                       = exprType e

-- | Returns the annotated type from the given pattern or 'Nothing' if no type
--   is annotated. Returns the return type for the 'PComb' constructor.
patternType' :: Pattern a -> Maybe (TypeExpr a)
patternType' (PComb _ ta _ _) = fmap returnType (typeAnnType ta)
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
  = let rhstes = catMaybes (rhsTypes' rhs)
        ptes = mapMaybe patternType' ps
        eqs = map (\te' -> foldr (FuncType x) te' ptes) rhstes
     in return ((te =.= tae) : map (tae =.=) eqs)
          ++= concatMapM (uncurry eqsPattern) (zip ptes ps)
          ++= eqsRhs rhs
eqsRule _  _                               = return []

-- | Returns the type expression equations for the given guard expression.
eqsGuard :: Expr a -> TIMonad a (TypeExprEqs a)
eqsGuard e = let x = exprAnn e
              in return [boolType x x =.= fromJust (exprType' e)]
                   ++= eqsExpr e

-- | Returns the type expression equations for the given right-hand side.
eqsRhs :: Rhs a -> TIMonad a (TypeExprEqs a)
eqsRhs (SimpleRhs e)      = eqsExpr e
eqsRhs (GuardedRhs _ eqs)
  = concatMapM (eqsExpr . snd) eqs ++= concatMapM (eqsGuard . fst) eqs

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
  = let ptes = mapMaybe patternType' ps
        rtae = returnType tae
     in return [te =.= rtae, tae =.= foldr (FuncType x) rtae ptes]
          ++= concatMapM (uncurry eqsPattern) (zip ptes ps)
eqsPattern te (PAs _ (TypeAnn tae) _ p)
  = let pte = fromJust (patternType' p)
     in return [te =.= tae, tae =.= pte] ++= eqsPattern pte p
eqsPattern te (PTuple x (TypeAnn tae) ps)
  = let ptes = mapMaybe patternType' ps
     in return [te =.= tae, tae =.= tupleType ptes x x]
          ++= concatMapM (uncurry eqsPattern) (zip ptes ps)
eqsPattern te (PList x (TypeAnn tae) ps)
  | null ps   = do tae' <- nextTVar x
                   return [te =.= tae, tae =.= listType tae' x x]
  | otherwise = let ptes = mapMaybe patternType' ps
                 in return ([te =.= tae, tae =.= listType (head ptes) x x]
                              ++ map (head ptes =.=) (tail ptes))
                      ++= concatMapM (uncurry eqsPattern) (zip ptes ps)
eqsPattern _  _                            = return []

funExprType :: Expr a -> Maybe (TypeExpr a)
funExprType (Var ta _)              = typeAnnType ta
funExprType (Lit ta _)              = typeAnnType ta
funExprType (Symbol ta _)           = typeAnnType ta
funExprType (Apply _ _ e1 _)        = funExprType e1
funExprType (InfixApply _ ta _ _ _) = fmap returnType (typeAnnType ta)
funExprType (Lambda _ ta _ _)       = typeAnnType ta
funExprType (Let _ ta _ _)          = typeAnnType ta
funExprType (DoExpr _ ta _)         = typeAnnType ta
funExprType (ListComp _ ta _ _)     = typeAnnType ta
funExprType (Case _ ta _ _)         = typeAnnType ta
funExprType (Typed _ ta _ _)        = typeAnnType ta
funExprType (IfThenElse _ ta _ _ _) = typeAnnType ta
funExprType (Tuple _ ta _)          = typeAnnType ta
funExprType (List _ ta _)           = typeAnnType ta

funArgs :: Expr a -> [Maybe (TypeExpr a)]
funArgs (Var ta _)              = []
funArgs (Lit ta _)              = []
funArgs (Symbol ta _)           = []
funArgs (Apply _ _ e1 e2)       = funArgs e1 ++ [exprType' e2]
funArgs (InfixApply _ ta _ _ _) = []
funArgs (Lambda _ ta _ _)       = []
funArgs (Let _ ta _ _)          = []
funArgs (DoExpr _ ta _)         = []
funArgs (ListComp _ ta _ _)     = []
funArgs (Case _ ta _ _)         = []
funArgs (Typed _ ta _ _)        = []
funArgs (IfThenElse _ ta _ _ _) = []
funArgs (Tuple _ ta _)          = []
funArgs (List _ ta _)           = []

-- | Returns the type expression equations for the given expression.
eqsExpr :: Expr a -> TIMonad a (TypeExprEqs a)
eqsExpr (Lit (TypeAnn te) (l, x))            = return [te =.= literalType l x x]
eqsExpr (Apply x (TypeAnn te) e1 e2)
  = let args = catMaybes (funArgs e1 ++ [exprType' e2])
        fType = fromJust (funExprType e1)
     in return [fType =.= foldr (FuncType x) te args]
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
  = let ptes = mapMaybe patternType' ps
        te' = fromJust (exprType' e)
     in return [te =.= foldr (FuncType x) te' ptes]
          ++= concatMapM (uncurry eqsPattern) (zip ptes ps)
          ++= eqsExpr e
eqsExpr DoExpr{}
  = throwError (TIError "do-expressions are not supported yet!")
eqsExpr ListComp{}
  = throwError (TIError "List comprehensions are not supported yet!")
eqsExpr (Case _ (TypeAnn te) e bs)
  = eqsExpr e ++= concatMapM (eqsBranch te (fromJust (exprType' e))) bs
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
  = let etes = mapMaybe exprType' es
     in return [te =.= tupleType etes x x]
          ++= concatMapM eqsExpr es
eqsExpr (List x (TypeAnn te) es)
  | null es   = do te' <- nextTVar x
                   return [te =.= listType te' x x]
  | otherwise = let etes = mapMaybe exprType' es
                 in return ((te =.= listType (head etes) x x)
                             : map (head etes =.=) (tail etes))
                      ++= concatMapM eqsExpr es
eqsExpr _                                    = return []

-- -----------------------------------------------------------------------------
-- Functions for type inference of abstract Haskell programs
-- -----------------------------------------------------------------------------

-- | Infers the given expression with the given list of programs.
inferExpr :: [Prog a] -> Expr a -> Either (TIError a) (Expr a)
inferExpr ps = inferExprEnv (getTypeEnv ps)

-- | Infers the given expression with the given type environment.
inferExprEnv :: TypeEnv a -> Expr a -> Either (TIError a) (Expr a)
inferExprEnv tenv e = evalState (runExceptT (inferExpr' e)) (initTIState tenv)

-- | Infers the given expression.
inferExpr' :: Expr a -> TIMonad a (Expr a)
inferExpr' e = do e' <- annExpr e
                  eqs <- eqsExpr e'
                  sub <- solve eqs
                  return (normalize normExpr (applyTESubstE sub e'))

-- | Returns the type expression of a typed function declaration or a type
--   variable if the function declaration is untyped.
funcDeclType :: FuncDecl a -> TypeExpr a
funcDeclType (Func x _ _ _ Untyped _)      = teVar 0 x
funcDeclType (Func _ _ _ _ (TypeSig te) _) = te

-- | Infers the given function declaration with the given list of programs. The
--   function declaration may not be contained in the given programs.
inferFuncDecl :: [Prog a] -> FuncDecl a -> Either (TIError a) (FuncDecl a)
inferFuncDecl ps = inferFuncDeclEnv (getTypeEnv ps)

-- | Infers the given function declaration with the given type environment.
inferFuncDeclEnv :: TypeEnv a -> FuncDecl a -> Either (TIError a) (FuncDecl a)
inferFuncDeclEnv tenv fd
  | DM.member (funcName fd) tenv = Left (TIError err)
  | otherwise
    = evalState (runExceptT (insertFunType (funcName fd) (funcDeclType fd)
                               >> annFunc fd >>= inferFunc))
                (initTIState tenv)
  where
    err = "Function already defined in the given program!"

-- | Infers the given already annotated function declaration.
inferFunc :: FuncDecl a -> TIMonad a (FuncDecl a)
inferFunc fd@(Func _ _ _ _ (TypeSig te) rs)
  = do eqs <- eqsRules te rs
       sub <- solve eqs
       let fd' = normalize normFuncDecl (applyTESubstFD sub fd)
       checkTooGeneral fd'
       return fd'

-- | Infers the given program with the 'Language.Haskell.Exts.Syntax'
--   representation using the given list of programs.
inferHSE :: [Prog a] -> Module a -> Either (TIError a) (Prog a)
inferHSE ps m = let tenv = getTypeEnv ps
                    p = hseToAH tenv m
                 in inferProgEnv (composeTypeEnv tenv (getTypeEnv [p])) p

-- | Infers the given program with the given list of programs.
inferProg :: [Prog a] -> Prog a -> Either (TIError a) (Prog a)
inferProg ps p = inferProgEnv (getTypeEnv (p:ps)) p

-- | Infers the given program with the given type environment.
inferProgEnv :: TypeEnv a -> Prog a -> Either (TIError a) (Prog a)
inferProgEnv tenv p = evalState (runExceptT (inferProg' p)) (initTIState tenv)

-- | Infers the given program.
inferProg' :: Prog a -> TIMonad a (Prog a)
inferProg' (Prog mn is tds fds)
  = let ntfds = filter (not . hasTypeSig) fds
     in do ntfds' <- inferNotTypedFuncs (fst mn) ntfds
           p' <- inferProg'' (Prog mn is tds (filter hasTypeSig fds))
           return (addNTFuncs p' ntfds')
  where
    addNTFuncs :: Prog a -> [FuncDecl a] -> Prog a
    addNTFuncs (Prog mn is tds fds) fds' = Prog mn is tds (fds ++ fds')

-- | Infers the given program with only typed function declarations.
inferProg'' :: Prog a -> TIMonad a (Prog a)
inferProg'' p = do Prog mn is tds fds <- annProg p
                   fds' <- mapM inferFunc fds
                   return (Prog mn is tds fds')

-- | Infers the given list of untyped function declarations within the module
--   with the given name.
inferNotTypedFuncs :: MName -> [FuncDecl a] -> TIMonad a [FuncDecl a]
inferNotTypedFuncs mn fds = concatMapM inferFuncGroup (depGraph mn fds)

-- | Infers the given group of untyped function declarations.
inferFuncGroup :: [FuncDecl a] -> TIMonad a [FuncDecl a]
inferFuncGroup fds
  = do fds' <- annFuncGroup fds
       eqs <- concatMapM (uncurry eqsRules)
                         [(te, rs) | Func _ _ _ _ (TypeSig te) rs <- fds']
       sub <- solve eqs
       nfds <- mapM (return . normalize normFuncDecl . applyTESubstFD sub) fds'
       extendTypeEnv [(qn, te) | Func _ (qn, _) _ _ (TypeSig te) _ <- nfds]
       return nfds

-- | Returns the part of the type expressions where the first type expression is
--   a too general variant of the second type expression or 'Nothing' if no such
--   part exists.
typeTooGeneral :: TypeExpr a -> TypeExpr a -> Maybe (TypeExpr a, TypeExpr a)
typeTooGeneral x@(TVar (vn1, _))  y@(TVar (vn2, _))
  | fst vn1 == fst vn2 = Nothing
  | otherwise          = Just (x, y)
typeTooGeneral x@(TVar _)         y@FuncType{}         = Just (x, y)
typeTooGeneral x@(TVar _)         y@TCons{}            = Just (x, y)
typeTooGeneral (FuncType _ t1 t2) (FuncType _ t1' t2')
  = typeTooGeneral' [(t1, t1'), (t2, t2')]
typeTooGeneral (TCons _ _ tes)    (TCons _ _ tes')
  = typeTooGeneral' (zip tes tes')
typeTooGeneral _                  _                    = Nothing

-- | Iterates 'typeTooGeneral' over a list of type expression pairs.
typeTooGeneral' :: [(TypeExpr a, TypeExpr a)] -> Maybe (TypeExpr a, TypeExpr a)
typeTooGeneral' []          = Nothing
typeTooGeneral' ((x, y):xs) = typeTooGeneral x y <|> typeTooGeneral' xs

-- | Checks whether the given function declaration has a too general type
--   signature compared to the infered type.
checkTooGeneral :: FuncDecl a -> TIMonad a ()
checkTooGeneral (Func _ (qn, _) _ _ _ rs)
  = do (tenv, _, _, _) <- get
       case lookupType qn tenv of
         Nothing -> return ()
         Just te ->
           let te' = normalize normTypeExpr te
            in maybe (return ())
                     (\(x, y) -> throwError (TITooGeneral x y))
                     (typeTooGeneral te' (head (catMaybes (rulesTypes rs))))
