{-|
  Library for representation of unification on first-order terms.

  This library implements a unification algorithm using reference tables.
-}

module TypeInference.Unification
  ( UnificationError (..)
  , unify, unifiable
  ) where

import           Data.Either                (isRight)
import           Data.List                  (mapAccumL)
import qualified Data.Map                   as DM
import           TypeInference.Substitution (Subst, emptySubst, extendSubst)
import           TypeInference.Term         (Term (..), TermEq, TermEqs, VarIdx,
                                             showVarIdx)

-- -----------------------------------------------------------------------------
-- Representation of unification errors
-- -----------------------------------------------------------------------------

-- | Representation of a unification error, parameterized over the kind of
--   function symbols, e.g., strings. In addition, the terms can be annotated
--   with any data type.
data UnificationError f a = Clash (Term f a) (Term f a)
                          | OccurCheck VarIdx (Term f a)

-- -----------------------------------------------------------------------------
-- Pretty-printing of unification errors
-- -----------------------------------------------------------------------------

-- | Transforms a unification error into a string representation.
showUnificationError :: Show f => UnificationError f a -> String
showUnificationError (Clash t1 t2)
  = unwords ["Clash:", show t1, "is not equal to", show t2] ++ "!"
showUnificationError (OccurCheck v t)
  = unwords ["OccurCheck:", showVarIdx v, "occurs in", show t] ++ "!"

instance Show f => Show (UnificationError f a) where
  show = showUnificationError

-- -----------------------------------------------------------------------------
-- Representation of internal data structures
-- -----------------------------------------------------------------------------

-- | An 'RTerm' is the unification algorithm's internal term representation. Its
--   'RTermVar' and 'RTermCons' constructors are similar to the 'TermVar' and
--   'TermCons' constructors of the original 'Term' data type, but it has an
--   additional 'Ref' constructor. This 'Ref' constructor is used to represent
--   references into a reference table.
data RTerm f a = Ref VarIdx
               | RTermVar a VarIdx
               | RTermCons a f [RTerm f a]

instance Eq f => Eq (RTerm f a) where
  (Ref v)            == (Ref v')             = v == v'
  (RTermVar _ v)     == (RTermVar _ v')      = v == v'
  (RTermCons _ c ts) == (RTermCons _ c' ts') = c == c' && ts == ts'
  _                  == _                    = False

-- | A reference table used to store the values referenced by 'Ref' terms
--   represented as a map from variables to 'RTerm's and parameterized over the
--   kind of function symbols, e.g., strings. In addition, the terms can be
--   annotated with any data type.
type RefTable f a = DM.Map VarIdx (RTerm f a)

-- | An 'RTerm' equation represented as a pair of 'RTerm's and parameterized
--   over the kind of function symbols, e.g., strings. In addition, the terms
--   can be annotated with any data type.
type REq f a = (RTerm f a, RTerm f a)

-- | Multiple 'RTerm' equations represented as a list of 'RTerm' equations and
--   parameterized over the kind of function symbols, e.g., strings. In
--   addition, the terms can be annotated with any data type.
type REqs f a = [REq f a]

-- -----------------------------------------------------------------------------
-- Definition of exported functions
-- -----------------------------------------------------------------------------

-- | Unifies a list of term equations. Returns either a unification error or a
--   substitution.
unify :: Eq f => TermEqs f a -> Either (UnificationError f a) (Subst f a)
unify eqs = let (rt, reqs) = termEqsToREqs eqs
             in either Left
                       (\(rt', reqs') -> Right (eqsToSubst rt' reqs'))
                       (unify' rt [] reqs)

-- | Checks whether a list of term equations can be unified.
unifiable :: Eq f => TermEqs f a -> Bool
unifiable = isRight . unify

-- -----------------------------------------------------------------------------
-- Conversion to internal structure
-- -----------------------------------------------------------------------------

-- | Converts a list of term equations into a list of 'RTerm' equations and
--   places references into a fresh reference table.
termEqsToREqs :: TermEqs f a -> (RefTable f a, REqs f a)
termEqsToREqs = mapAccumL termEqToREq DM.empty

-- | Converts a term equation into an 'RTerm' equation. The given reference
--   table is used to store references.
termEqToREq :: RefTable f a -> TermEq f a -> (RefTable f a, REq f a)
termEqToREq rt (l, r) = let (rt1, l') = termToRTerm rt l
                            (rt2, r') = termToRTerm rt1 r
                         in (rt2, (l', r'))

-- | Converts a term to an 'RTerm', placing all variable terms in the given
--   reference table and replacing them by references inside the result 'RTerm'.
termToRTerm :: RefTable f a -> Term f a -> (RefTable f a, RTerm f a)
termToRTerm rt (TermVar a v)     = (DM.insert v (RTermVar a v) rt, Ref v)
termToRTerm rt (TermCons a c ts) = let (rt', ts') = mapAccumL termToRTerm rt ts
                                    in (rt', RTermCons a c ts')

-- -----------------------------------------------------------------------------
-- Conversion from internal structure
-- -----------------------------------------------------------------------------

-- | Converts a list of 'RTerm' equations to a substitution by turning every
--   equation of the form @(RTermVar _ v, t)@ or @(t, RTermVar _ v)@ into a
--   mapping @(v, t)@. Equations that do not have a variable term on either side
--   are ignored. Works on 'RTerm's, dereferences all 'Ref's.
eqsToSubst :: RefTable f a -> REqs f a -> Subst f a
eqsToSubst _  []           = emptySubst
eqsToSubst rt ((l, r):eqs)
  = case l of
      Ref _        -> eqsToSubst rt ((deref rt l, r):eqs)
      RTermVar _ v -> extendSubst v (rTermToTerm rt r) (eqsToSubst rt eqs)
      _            ->
        case r of
          Ref _        -> eqsToSubst rt ((l, deref rt r):eqs)
          RTermVar _ v -> extendSubst v (rTermToTerm rt l) (eqsToSubst rt eqs)
          _            -> eqsToSubst rt eqs

-- | Converts an 'RTerm' to a term by dereferencing all references inside the
--   'RTerm'. The given reference table is used for reference lookups.
rTermToTerm :: RefTable f a -> RTerm f a -> Term f a
rTermToTerm rt t@(Ref _)          = rTermToTerm rt (deref rt t)
rTermToTerm _  (RTermVar a v)     = TermVar a v
rTermToTerm rt (RTermCons a c ts) = TermCons a c (map (rTermToTerm rt) ts)

-- | Dereferences an 'RTerm' by following chained references. Simply returns the
--   same value for 'RTermVar' and 'RTermCons'. The given reference table is
--   used for reference lookups.
deref :: RefTable f a -> RTerm f a -> RTerm f a
deref rt (Ref i) = case DM.lookup i rt of
                     Nothing -> error ("deref: " ++ showVarIdx i)
                     Just t  -> case t of
                                  Ref _ -> deref rt t
                                  _     -> t
deref _  t       = t

-- -----------------------------------------------------------------------------
-- Unification algorithm
-- -----------------------------------------------------------------------------

-- | Internal unification function, the core of the algorithm.
unify' :: Eq f => RefTable f a -> REqs f a -> REqs f a
       -> Either (UnificationError f a) (RefTable f a, REqs f a)
unify' rt sub []              = Right (rt, sub)
unify' rt sub (eq@(l, r):eqs)
  = case eq of
      (RTermVar _ v, RTermCons{})               -> elim rt sub v r eqs
      (RTermCons{}, RTermVar _ v)               -> elim rt sub v l eqs
      (RTermVar _ v, RTermVar _ v') | v == v'   -> unify' rt sub eqs
                                    | otherwise -> elim rt sub v r eqs
      (RTermCons _ c1 ts1, RTermCons _ c2 ts2)
        | c1 == c2  -> unify' rt sub (zip ts1 ts2 ++ eqs)
        | otherwise -> Left (Clash (rTermToTerm rt l) (rTermToTerm rt r))
      _             -> unify' rt sub ((deref rt l, deref rt r):eqs)

-- | Substitutes a variable by a term inside a list of equations that have yet
--   to be unified and the right-hand sides of all equations of the result list.
--   Also adds a mapping from that variable to that term to the result list.
elim :: Eq f => RefTable f a -> REqs f a -> VarIdx -> RTerm f a -> REqs f a
     -> Either (UnificationError f a) (RefTable f a, REqs f a)
elim rt sub v t eqs
  | dependsOn rt (RTermVar undefined v) t
    = Left (OccurCheck v (rTermToTerm rt t))
  | otherwise
    = case t of
        Ref _         -> error "elim"
        RTermVar a v' -> let rt' = DM.insert v (Ref v') rt
                          in unify' rt' ((RTermVar a v, Ref v'):sub) eqs
        _             -> unify' (DM.insert v t rt)
                                ((RTermVar undefined v, t):sub)
                                eqs

-- | Checks whether the first term occurs as a subterm of the second term.
dependsOn :: Eq f => RefTable f a -> RTerm f a -> RTerm f a -> Bool
dependsOn rt l r = l /= r && dependsOn' r
  where
    dependsOn' t@(Ref _)          = deref rt t == l
    dependsOn' t@(RTermVar _ _)   = t == l
    dependsOn' (RTermCons _ _ ts) = any dependsOn' ts
