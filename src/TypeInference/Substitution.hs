module TypeInference.Substitution
  ( Subst
  , showSubst, emptySubst, extendSubst, listToSubst, lookupSubst, applySubst
  , applySubstEq, applySubstEqs, restrictSubst, composeSubst
  ) where

import qualified Data.Map as DM
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import TypeInference.Term (VarIdx, Term (..), TermEq, TermEqs, showVarIdx)

-- ---------------------------------------------------------------------------
-- Representation of substitutions on first-order terms
-- ---------------------------------------------------------------------------

--- A substitution represented as a finite map from variables to terms and
--- parameterized over the kind of function symbols, e.g., strings.
type Subst f = DM.Map VarIdx (Term f)

-- ---------------------------------------------------------------------------
-- Pretty-printing of substitutions on first-order terms
-- ---------------------------------------------------------------------------

-- \x21a6 = RIGHTWARDS ARROW FROM BAR

--- Transforms a substitution into a string representation.
showSubst :: Show f => Subst f -> String
showSubst sub = "{" ++ (intercalate "," (map showMapping (DM.toList sub)))
                    ++ "}"
  where
    showMapping :: Show f => (VarIdx, Term f) -> String
    showMapping (v, t) = (showVarIdx v) ++ " \8614 "  ++ (show t)

-- ---------------------------------------------------------------------------
-- Functions for substitutions on first-order terms
-- ---------------------------------------------------------------------------

--- The empty substitution.
emptySubst :: Subst f
emptySubst = DM.empty

--- Extends a substitution with a new mapping from the given variable to the
--- given term. An already existing mapping with the same variable will be
--- thrown away.
extendSubst :: Subst f -> VarIdx -> Term f -> Subst f
extendSubst sub v t = DM.insert v t sub

--- Returns a substitution that contains all the mappings from the given list.
--- For multiple mappings with the same variable, the last corresponding
--- mapping of the given list is taken.
listToSubst :: [(VarIdx, Term f)] -> Subst f
listToSubst = DM.fromList

--- Returns the term mapped to the given variable in a substitution or
--- `Nothing` if no such mapping exists.
lookupSubst :: Subst f -> VarIdx -> Maybe (Term f)
lookupSubst = flip DM.lookup

--- Applies a substitution to the given term.
applySubst :: Subst f -> Term f -> Term f
applySubst sub t@(TermVar _ v)    = fromMaybe t (lookupSubst sub v)
applySubst sub (TermCons ss c ts) = TermCons ss c (map (applySubst sub) ts)

--- Applies a substitution to both sides of the given term equation.
applySubstEq :: Subst f -> TermEq f -> TermEq f
applySubstEq sub = both (applySubst sub)

--- Applies a substitution to every term equation in the given list.
applySubstEqs :: Subst f -> TermEqs f -> TermEqs f
applySubstEqs sub = map (applySubstEq sub)

--- Returns a new substitution with only those mappings from the given
--- substitution whose variable is in the given list of variables.
restrictSubst :: Subst f -> [VarIdx] -> Subst f
restrictSubst sub vs
  = listToSubst [(v, t) | v <- vs, (Just t) <- [lookupSubst sub v]]

--- Composes the first substitution `phi` with the second substitution
--- `sigma`. The resulting substitution `sub` fulfills the property
--- `sub(t) = phi(sigma(t))` for a term `t`. Mappings in the first
--- substitution shadow those in the second.
composeSubst :: Subst f -> Subst f -> Subst f
composeSubst phi sigma = DM.union phi (DM.map (\t -> applySubst phi t) sigma)

-- -----------------------------------------------------------------------------
-- Definition of helper functions
-- -----------------------------------------------------------------------------

-- Applies a function to both components of a tuple.
both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)