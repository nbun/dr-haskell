module TypeInference.Unification
  ( UnificationError (..)
  , showUnificationError, unify, unifiable
  ) where

import Data.Map (Map, empty, insert, lookup)
import Data.List (mapAccumL)
import TypeInference.Substitution (Subst, emptySubst, extendSubst, showSubst)
import TypeInference.Term (VarIdx, Term (..), TermEq, TermEqs, showVarIdx, showTermSS)
import Prelude hiding (lookup)
import Language.Haskell.Exts.SrcLoc (SrcSpan (..))

isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True

-- ---------------------------------------------------------------------------
-- Representation of unification errors
-- ---------------------------------------------------------------------------

--- Representation of a unification error, parameterized over the kind of
--- function symbols, e.g., strings.
---
--- @cons Clash t1 t2    - The constructor term 't1' is supposed to be equal
---                        to the constructor term 't2' but has a different
---                        constructor.
--- @cons OccurCheck v t - The variable 'v' is supposed to be equal to the
---                        term 't' in which it occurs as a subterm.
data UnificationError f = Clash (Term f) (Term f) | OccurCheck VarIdx (Term f)

-- ---------------------------------------------------------------------------
-- Pretty-printing of unification errors
-- ---------------------------------------------------------------------------

getSS :: Term f -> SrcSpan
getSS (TermVar ss _)    = ss
getSS (TermCons ss _ _) = ss

--- Transforms a unification error into a string representation.
showUnificationError :: Show f => UnificationError f -> String
showUnificationError (Clash t1 t2)    = "Clash: " ++ (showTermSS (getSS t1)) ++ ":" ++ (show t1)
                                            ++ " is not equal to "
                                            ++ (showTermSS (getSS t2)) ++ ":" ++ (show t2) ++ "."
showUnificationError (OccurCheck v t) = "OccurCheck: " ++ (showVarIdx v)
                                            ++ " occurs in " ++ (showTermSS (getSS t)) ++ ":" ++ (show t)
                                            ++ "."

-- ---------------------------------------------------------------------------
-- Representation of internal data structures
-- ---------------------------------------------------------------------------

--- An 'RTerm' is the unification algorithm's internal term representation.
--- Its 'RTermVar' and 'RTermCons' constructors are similar to the 'TermVar'
--- and 'TermCons' constructors of the original 'Term' data type, but it has
--- an additional 'Ref' constructor. This 'Ref' constructor is used to
--- represent references into a reference table.
data RTerm f = Ref VarIdx | RTermVar SrcSpan VarIdx | RTermCons SrcSpan f [RTerm f]
  deriving Eq

--- A reference table used to store the values referenced by 'Ref' terms
--- represented as a finite map from variables to 'RTerm's and parameterized
--- over the kind of function symbols, e.g., strings.
type RefTable f = Map VarIdx (RTerm f)

--- An 'RTerm' equation represented as a pair of 'RTerm's and parameterized
--- over the kind of function symbols, e.g., strings.
type REq f = (RTerm f, RTerm f)

--- Multiple 'RTerm' equations represented as a list of 'RTerm' equations and
--- parameterized over the kind of function symbols, e.g., strings.
type REqs f = [REq f]

-- ---------------------------------------------------------------------------
-- Definition of exported functions
-- ---------------------------------------------------------------------------

--- Unifies a list of term equations. Returns either a unification error or a
--- substitution.
unify :: Eq f => TermEqs f -> Either (UnificationError f) (Subst f)
unify eqs = let (rt, reqs) = termEqsToREqs eqs
             in either Left
                       (\(rt', reqs') -> Right (eqsToSubst rt' reqs'))
                       (unify' rt [] reqs)

--- Checks whether a list of term equations can be unified.
unifiable :: Eq f => TermEqs f -> Bool
unifiable = isRight . unify

-- ---------------------------------------------------------------------------
-- Conversion to internal structure
-- ---------------------------------------------------------------------------

--- Converts a list of term equations into a list of 'RTerm' equations and
--- places references into a fresh reference table.
termEqsToREqs :: TermEqs f -> (RefTable f, REqs f)
termEqsToREqs = mapAccumL termEqToREq empty

--- Converts a term equation into an 'RTerm' equation. The given reference
--- table is used to store references.
termEqToREq :: RefTable f -> TermEq f -> (RefTable f, REq f)
termEqToREq rt (l, r) = let (rt1, l') = termToRTerm rt l
                            (rt2, r') = termToRTerm rt1 r
                         in (rt2, (l', r'))

--- Converts a term to an 'RTerm', placing all variable terms in the given
--- reference table and replacing them by references inside the result
--- 'RTerm'.
termToRTerm :: RefTable f -> Term f -> (RefTable f, RTerm f)
termToRTerm rt (TermVar ss v)     = (insert v (RTermVar ss v) rt, Ref v)
termToRTerm rt (TermCons ss c ts) = let (rt', ts') = mapAccumL termToRTerm rt ts
                                    in (rt', RTermCons ss c ts')

-- ---------------------------------------------------------------------------
-- Conversion from internal structure
-- ---------------------------------------------------------------------------

--- Converts a list of 'RTerm' equations to a substitution by turning every
--- equation of the form '(RTermVar v, t)' or '(t, RTermVar v)' into a mapping
--- '(v, t)'. Equations that do not have a variable term on either side are
--- ignored. Works on 'RTerm's, dereferences all 'Ref's.
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

--- Converts an 'RTerm' to a term by dereferencing all references inside the
--- 'RTerm'. The given reference table is used for reference lookups.
rTermToTerm :: RefTable f -> RTerm f -> Term f
rTermToTerm rt t@(Ref _)           = rTermToTerm rt (deref rt t)
rTermToTerm _  (RTermVar ss v)     = TermVar ss v
rTermToTerm rt (RTermCons ss c ts) = TermCons ss c (map (rTermToTerm rt) ts)

--- Dereferences an 'RTerm' by following chained references. Simply returns
--- the same value for 'RTermVar' and 'RTermCons'. The given reference table
--- is used for reference lookups.
deref :: RefTable f -> RTerm f -> RTerm f
deref rt (Ref i)           = case lookup i rt of
                               Nothing  -> error ("deref: " ++ (show i))
                               (Just t) -> case t of
                                             (Ref _)           -> deref rt t
                                             (RTermVar _ _)    -> t
                                             (RTermCons _ _ _) -> t
deref _  t@(RTermVar _ _)    = t
deref _  t@(RTermCons _ _ _) = t

-- ---------------------------------------------------------------------------
-- Unification algorithm
-- ---------------------------------------------------------------------------

--- Internal unification function, the core of the algorithm.
unify' :: Eq f => RefTable f -> REqs f -> REqs f
       -> Either (UnificationError f) (RefTable f, REqs f)
-- No equations left, we are done.
unify' rt sub []              = Right (rt, sub)
unify' rt sub (eq@(l, r):eqs)
  = case eq of
      -- Substitute the variable by the constructor term.
      (RTermVar _ v, RTermCons _ _ _)           -> elim rt sub v r eqs
      (RTermCons _ _ _, RTermVar _ v)           -> elim rt sub v l eqs
      -- If both variables are equal, simply remove the equation.
      -- Otherwise substitute the first variable by the second variable.
      (RTermVar _ v, RTermVar _ v') | v == v'   -> unify' rt sub eqs
                                    | otherwise -> elim rt sub v r eqs
      -- If both constructors have the same name, equate their arguments.
      -- Otherwise fail with a clash.
      (RTermCons _ c1 ts1, RTermCons _ c2 ts2)
        | c1 == c2  -> unify' rt sub ((zip ts1 ts2) ++ eqs)
        | otherwise -> Left (Clash (rTermToTerm rt l) (rTermToTerm rt r))
      -- If we encounter a Ref, simply dereference it and try again.
      _ -> unify' rt sub ((deref rt l, deref rt r):eqs)

--- Substitutes a variable by a term inside a list of equations that have
--- yet to be unified and the right-hand sides of all equations of the result
--- list. Also adds a mapping from that variable to that term to the result
--- list.
elim :: Eq f => RefTable f -> REqs f -> VarIdx -> RTerm f -> REqs f
     -> Either (UnificationError f) (RefTable f, REqs f)
elim rt sub v t eqs
  | dependsOn rt (RTermVar emptySS v) t = Left (OccurCheck v (rTermToTerm rt t))
  | otherwise
    = case t of
        (Ref _)         -> error "elim"
        -- Make sure to place a Ref in the reference table and substitution,
        -- not the RTermVar itself.
        (RTermVar ss v')   -> let rt' = insert v (Ref v') rt
                               in unify' rt' ((RTermVar ss v, Ref v'):sub) eqs
        (RTermCons _ _ _) -> unify' (insert v t rt) ((RTermVar emptySS v, t):sub) eqs

--- Checks whether the first term occurs as a subterm of the second term.
dependsOn :: Eq f => RefTable f -> RTerm f -> RTerm f -> Bool
dependsOn rt l r = (l /= r) && (dependsOn' r)
  where
    -- dependsOn' :: RTerm f -> Bool
    dependsOn' x@(Ref _)          = (deref rt x) == l
    dependsOn' t@(RTermVar _ _)   = l == t
    dependsOn' (RTermCons _ _ ts) = or (map dependsOn' ts)

emptySS :: SrcSpan
emptySS = SrcSpan "" 0 0 0 0