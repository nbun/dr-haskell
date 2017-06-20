module TypeInference.Substitution
  ( Subst
  , showSubst, emptySubst, extendSubst, listToSubst, substToList, lookupSubst
  , applySubst, applySubstEq, applySubstEqs, restrictSubst, composeSubst
  ) where

import Data.List (intercalate)
import qualified Data.Map as DM
import Data.Maybe (fromMaybe)
import TypeInference.Term (VarIdx, Term (..), TermEq, TermEqs, showVarIdx)

-- -----------------------------------------------------------------------------
-- Representation of substitutions on first-order terms
-- -----------------------------------------------------------------------------

-- A substitution represented as a map from variables to terms and parameterized
-- over the kind of function symbols, e.g., strings.
type Subst f = DM.Map VarIdx (Term f)

-- -----------------------------------------------------------------------------
-- Pretty-printing of substitutions on first-order terms
-- -----------------------------------------------------------------------------

-- \8614 = RIGHTWARDS ARROW FROM BAR

-- Transforms a substitution into a string representation.
showSubst :: Show f => Subst f -> String
showSubst sub = "{" ++ (intercalate "," (map showMapping (DM.toList sub)))
                    ++ "}"
  where
    showMapping :: Show f => (VarIdx, Term f) -> String
    showMapping (v, t) = (showVarIdx v) ++ " \8614 "  ++ (show t)

-- -----------------------------------------------------------------------------
-- Functions for substitutions on first-order terms
-- -----------------------------------------------------------------------------

-- The empty substitution.
emptySubst :: Subst f
emptySubst = DM.empty

-- Extends a substitution with a new mapping from the given variable to the
-- given term. An already existing mapping with the same variable will be thrown
-- away.
extendSubst :: Subst f -> VarIdx -> Term f -> Subst f
extendSubst sub v t = DM.insert v t sub

-- Returns a substitution that contains all the mappings from the given list.
-- For multiple mappings with the same variable, the last corresponding mapping
-- of the given list is taken.
listToSubst :: [(VarIdx, Term f)] -> Subst f
listToSubst = DM.fromList

-- Returns a list with all mappings from the given substitution.
substToList :: Subst f -> [(VarIdx, Term f)]
substToList = DM.toList

-- Returns the term mapped to the given variable in a substitution or 'Nothing'
-- if no such mapping exists.
lookupSubst :: Subst f -> VarIdx -> Maybe (Term f)
lookupSubst = flip DM.lookup

-- Applies a substitution to the given term. The source span information will
-- not be updated with respect to the replaced variables.
applySubst :: Subst f -> Term f -> Term f
applySubst sub t@(TermVar _ v)     = fromMaybe t (lookupSubst sub v)
applySubst sub (TermCons ssi c ts) = TermCons ssi c (map (applySubst sub) ts)

-- Applies a substitution to both sides of the given term equation. The source
-- span information will not be updated with respect to the replaced variables.
applySubstEq :: Subst f -> TermEq f -> TermEq f
applySubstEq sub = both (applySubst sub)

-- Applies a substitution to every term equation in the given list. The source
-- span information will not be updated with respect to the replaced variables.
applySubstEqs :: Subst f -> TermEqs f -> TermEqs f
applySubstEqs sub = map (applySubstEq sub)

-- Returns a new substitution with only those mappings from the given
-- substitution whose variable is in the given list of variables.
restrictSubst :: Subst f -> [VarIdx] -> Subst f
restrictSubst sub vs
  = listToSubst [(v, t) | v <- vs, (Just t) <- [lookupSubst sub v]]

-- Composes the first substitution 'phi' with the second substitution 'sigma'.
-- The resulting substitution 'sub' fulfills the property
-- 'sub(t) = phi(sigma(t))' for a term 't'. Mappings in the first substitution
-- shadow those in the second. The source span information will not be updated
-- with respect to the replaced variables.
composeSubst :: Subst f -> Subst f -> Subst f
composeSubst phi sigma = DM.union phi (DM.map (applySubst phi) sigma)

-- -----------------------------------------------------------------------------
-- Definition of helper functions
-- -----------------------------------------------------------------------------

-- Applies a function to both components of a tuple.
both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)