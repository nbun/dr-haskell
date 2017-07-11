{-|
  Library for representation of substitutions on first-order terms.
-}

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

-- | A substitution represented as a map from variables to terms and
--   parameterized over the kind of function symbols, e.g., strings. In
--   addition, the terms can be annotated with any data type.
type Subst f a = DM.Map VarIdx (Term f a)

-- -----------------------------------------------------------------------------
-- Pretty-printing of substitutions on first-order terms
-- -----------------------------------------------------------------------------

-- \8614 = RIGHTWARDS ARROW FROM BAR

-- | Transforms a substitution into a string representation.
showSubst :: Show f => Subst f a -> String
showSubst sub = "{" ++ intercalate "," (map showMapping (substToList sub))
                    ++ "}"
  where
    showMapping :: Show f => (VarIdx, Term f a) -> String
    showMapping (v, t) = showVarIdx v ++ " \8614 " ++ show t

-- -----------------------------------------------------------------------------
-- Functions for substitutions on first-order terms
-- -----------------------------------------------------------------------------

-- | The empty substitution.
emptySubst :: Subst f a
emptySubst = DM.empty

-- | Extends a substitution with a new mapping from the given variable to the
--   given term. An already existing mapping with the same variable will be
--   thrown away.
extendSubst :: VarIdx -> Term f a -> Subst f a -> Subst f a
extendSubst = DM.insert

-- | Returns a substitution that contains all the mappings from the given list.
--   For multiple mappings with the same variable, the last corresponding
--   mapping of the given list is taken.
listToSubst :: [(VarIdx, Term f a)] -> Subst f a
listToSubst = DM.fromList

-- | Returns a list with all mappings from the given substitution.
substToList :: Subst f a -> [(VarIdx, Term f a)]
substToList = DM.toList

-- | Returns the term mapped to the given variable in a substitution or
--   'Nothing' if no such mapping exists.
lookupSubst :: VarIdx -> Subst f a -> Maybe (Term f a)
lookupSubst = DM.lookup

-- | Applies a substitution to the given term.
applySubst :: Subst f a -> Term f a -> Term f a
applySubst sub t@(TermVar _ v)   = fromMaybe t (lookupSubst v sub)
applySubst sub (TermCons a c ts) = TermCons a c (map (applySubst sub) ts)

-- | Applies a substitution to both sides of the given term equation.
applySubstEq :: Subst f a -> TermEq f a -> TermEq f a
applySubstEq sub = both (applySubst sub)

-- | Applies a substitution to every term equation in the given list.
applySubstEqs :: Subst f a -> TermEqs f a -> TermEqs f a
applySubstEqs sub = map (applySubstEq sub)

-- | Returns a new substitution with only those mappings from the given
--   substitution whose variable is in the given list of variables.
restrictSubst :: Subst f a -> [VarIdx] -> Subst f a
restrictSubst sub vs
  = listToSubst [(v, t) | v <- vs, Just t <- [lookupSubst v sub]]

-- | Composes the first substitution @phi@ with the second substitution @sigma@.
--   The resulting substitution @sub@ fulfills the property
--   @sub(t) = phi(sigma(t))@ for a term @t@. Mappings in the first substitution
--   shadow those in the second.
composeSubst :: Subst f a -> Subst f a -> Subst f a
composeSubst phi sigma = DM.union phi (DM.map (applySubst phi) sigma)

-- -----------------------------------------------------------------------------
-- Definition of helper functions
-- -----------------------------------------------------------------------------

-- | Applies a function to both components of a tuple.
both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)