module TypeInference.Unification
  ( UnificationError (..)
  , showUnificationError, unify, unifiable
  ) where

import Data.Either (isRight)
import qualified Data.Map as DM
import Data.List (mapAccumL)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import TypeInference.Substitution (Subst, emptySubst, extendSubst)
import TypeInference.Term (VarIdx, Term (..), TermEq, TermEqs, showVarIdx)

-- -----------------------------------------------------------------------------
-- Representation of unification errors
-- -----------------------------------------------------------------------------

-- Representation of a unification error, parameterized over the kind of
-- function symbols, e.g., strings.
data UnificationError f = Clash (Term f) (Term f)
                        | OccurCheck VarIdx (Term f)

-- -----------------------------------------------------------------------------
-- Pretty-printing of unification errors
-- -----------------------------------------------------------------------------

-- Transforms a unification error into a string representation.
showUnificationError :: Show f => UnificationError f -> String
showUnificationError (Clash t1 t2)    = "Clash: " ++ (show t1)
                                                  ++ " is not equal to "
                                                  ++ (show t2)
                                                  ++ "!"
showUnificationError (OccurCheck v t) = "OccurCheck: " ++ (showVarIdx v)
                                                       ++ " occurs in "
                                                       ++ (show t)
                                                       ++ "!"

-- -----------------------------------------------------------------------------
-- Representation of internal data structures
-- -----------------------------------------------------------------------------

-- An 'RTerm' is the unification algorithm's internal term representation. Its
-- 'RTermVar' and 'RTermCons' constructors are similar to the 'TermVar' and
-- 'TermCons' constructors of the original 'Term' data type, but it has an
-- additional 'Ref' constructor. This 'Ref' constructor is used to represent
-- references into a reference table.
data RTerm f = Ref VarIdx
             | RTermVar SrcSpanInfo VarIdx
             | RTermCons SrcSpanInfo f [RTerm f]

-- A reference table used to store the values referenced by 'Ref' terms
-- represented as a map from variables to 'RTerm's and parameterized over the
-- kind of function symbols, e.g., strings.
type RefTable f = DM.Map VarIdx (RTerm f)

-- An 'RTerm' equation represented as a pair of 'RTerm's and parameterized over
-- the kind of function symbols, e.g., strings.
type REq f = (RTerm f, RTerm f)

-- Multiple 'RTerm' equations represented as a list of 'RTerm' equations and
-- parameterized over the kind of function symbols, e.g., strings.
type REqs f = [REq f]

instance Eq f => Eq (RTerm f) where
  (Ref v)            == (Ref v')             = v == v'
  (RTermVar _ v)     == (RTermVar _ v')      = v == v'
  (RTermCons _ c ts) == (RTermCons _ c' ts') = (c == c') && (ts == ts')
  _                  == _                    = False

-- -----------------------------------------------------------------------------
-- Definition of exported functions
-- -----------------------------------------------------------------------------

-- Unifies a list of term equations. Returns either a unification error or a
-- substitution.
unify :: Eq f => TermEqs f -> Either (UnificationError f) (Subst f)
unify eqs = let (rt, reqs) = termEqsToREqs eqs
             in either Left
                       (\(rt', reqs') -> Right (eqsToSubst rt' reqs'))
                       (unify' rt [] reqs)

-- Checks whether a list of term equations can be unified.
unifiable :: Eq f => TermEqs f -> Bool
unifiable = isRight . unify

-- -----------------------------------------------------------------------------
-- Conversion to internal structure
-- -----------------------------------------------------------------------------

-- Converts a list of term equations into a list of 'RTerm' equations and places
-- references into a fresh reference table.
termEqsToREqs :: TermEqs f -> (RefTable f, REqs f)
termEqsToREqs = mapAccumL termEqToREq DM.empty

-- Converts a term equation into an 'RTerm' equation. The given reference table
-- is used to store references.
termEqToREq :: RefTable f -> TermEq f -> (RefTable f, REq f)
termEqToREq rt (l, r) = let (rt1, l') = termToRTerm rt l
                            (rt2, r') = termToRTerm rt1 r
                         in (rt2, (l', r'))

-- Converts a term to an 'RTerm', placing all variable terms in the given
-- reference table and replacing them by references inside the result 'RTerm'.
termToRTerm :: RefTable f -> Term f -> (RefTable f, RTerm f)
termToRTerm rt (TermVar ssi v)     = (DM.insert v (RTermVar ssi v) rt, Ref v)
termToRTerm rt (TermCons ssi c ts)
  = let (rt', ts') = mapAccumL termToRTerm rt ts
     in (rt', RTermCons ssi c ts')

-- -----------------------------------------------------------------------------
-- Conversion from internal structure
-- -----------------------------------------------------------------------------

-- Converts a list of 'RTerm' equations to a substitution by turning every
-- equation of the form '(RTermVar v, t)' or '(t, RTermVar v)' into a mapping
-- '(v, t)'. Equations that do not have a variable term on either side are
-- ignored. Works on 'RTerm's, dereferences all 'Ref's.
eqsToSubst :: RefTable f -> REqs f -> Subst f
eqsToSubst _  []           = emptySubst
eqsToSubst rt ((l, r):eqs)
  = case l of
      (Ref _)           -> eqsToSubst rt ((deref rt l, r):eqs)
      (RTermVar _ v)    -> extendSubst (eqsToSubst rt eqs) v (rTermToTerm rt r)
      (RTermCons _ _ _) ->
        case r of
          (Ref _)        -> eqsToSubst rt ((l, deref rt r):eqs)
          (RTermVar _ v) -> extendSubst (eqsToSubst rt eqs) v (rTermToTerm rt l)
          _              -> eqsToSubst rt eqs

-- Converts an 'RTerm' to a term by dereferencing all references inside the
-- 'RTerm'. The given reference table is used for reference lookups.
rTermToTerm :: RefTable f -> RTerm f -> Term f
rTermToTerm rt t@(Ref _)            = rTermToTerm rt (deref rt t)
rTermToTerm _  (RTermVar ssi v)     = TermVar ssi v
rTermToTerm rt (RTermCons ssi c ts) = TermCons ssi c (map (rTermToTerm rt) ts)

-- Dereferences an 'RTerm' by following chained references. Simply returns the
-- same value for 'RTermVar' and 'RTermCons'. The given reference table is used
-- for reference lookups.
deref :: RefTable f -> RTerm f -> RTerm f
deref rt (Ref i)             = case DM.lookup i rt of
                                 Nothing  -> error ("deref: " ++ (show i))
                                 (Just t) -> case t of
                                               (Ref _)           -> deref rt t
                                               (RTermVar _ _)    -> t
                                               (RTermCons _ _ _) -> t
deref _  t@(RTermVar _ _)    = t
deref _  t@(RTermCons _ _ _) = t

-- -----------------------------------------------------------------------------
-- Unification algorithm
-- -----------------------------------------------------------------------------

-- Internal unification function, the core of the algorithm.
unify' :: Eq f => RefTable f -> REqs f -> REqs f
       -> Either (UnificationError f) (RefTable f, REqs f)
unify' rt sub []              = Right (rt, sub)
unify' rt sub (eq@(l, r):eqs)
  = case eq of
      (RTermVar _ v, RTermCons _ _ _)           -> elim rt sub v r eqs
      (RTermCons _ _ _, RTermVar _ v)           -> elim rt sub v l eqs
      (RTermVar _ v, RTermVar _ v') | v == v'   -> unify' rt sub eqs
                                    | otherwise -> elim rt sub v r eqs
      (RTermCons _ c1 ts1, RTermCons _ c2 ts2)
        | c1 == c2  -> unify' rt sub ((zip ts1 ts2) ++ eqs)
        | otherwise -> Left (Clash (rTermToTerm rt l) (rTermToTerm rt r))
      _ -> unify' rt sub ((deref rt l, deref rt r):eqs)

-- Substitutes a variable by a term inside a list of equations that have yet to
-- be unified and the right-hand sides of all equations of the result list. Also
-- adds a mapping from that variable to that term to the result list.
elim :: Eq f => RefTable f -> REqs f -> VarIdx -> RTerm f -> REqs f
     -> Either (UnificationError f) (RefTable f, REqs f)
elim rt sub v t eqs
  | dependsOn rt (RTermVar undefined v) t
    = Left (OccurCheck v (rTermToTerm rt t))
  | otherwise
    = case t of
        (Ref _)           -> error "elim"
        (RTermVar ssi v') -> let rt' = DM.insert v (Ref v') rt
                              in unify' rt' ((RTermVar ssi v, Ref v'):sub) eqs
        (RTermCons _ _ _)
          -> unify' (DM.insert v t rt) ((RTermVar undefined v, t):sub) eqs

-- Checks whether the first term occurs as a subterm of the second term.
dependsOn :: Eq f => RefTable f -> RTerm f -> RTerm f -> Bool
dependsOn rt l r = (l /= r) && (dependsOn' r)
  where
    dependsOn' x@(Ref _)          = (deref rt x) == l
    dependsOn' t@(RTermVar _ _)   = t == l
    dependsOn' (RTermCons _ _ ts) = or (map dependsOn' ts)