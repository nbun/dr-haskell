{-|
  Library for representation of first-order terms.

  This library is the basis of other libraries for the manipulation of
  first-order terms, e.g., unification of terms. Therefore, this library also
  defines other structures, like term equations.
-}

module TypeInference.Term
  ( VarIdx, Term (..), TermEq, TermEqs
  , showVarIdx, showTermEq, showTermEqs
  ) where

import Data.List (intercalate)
import Goodies (parensIf)

-- -----------------------------------------------------------------------------
-- Representation of first-order terms and term equations
-- -----------------------------------------------------------------------------

-- | A variable represented as an integer greater than or equal to zero.
type VarIdx = Int

-- | Representation of a first-order term, parameterized over the kind of
--   function symbols, e.g., strings. In addition, a term can be annotated with
--   any data type.
data Term f a = TermVar a VarIdx
              | TermCons a f [Term f a]

-- | A term equation represented as a pair of terms and parameterized over the
--   kind of function symbols, e.g., strings. In addition, the terms can be
--   annotated with any data type.
type TermEq f a = (Term f a, Term f a)

-- | Multiple term equations represented as a list of term equations and
--   parameterized over the kind of function symbols, e.g., strings. In
--   addition, the terms can be annotated with any data type.
type TermEqs f a = [TermEq f a]

-- -----------------------------------------------------------------------------
-- Pretty-printing of first-order terms and term equations
-- -----------------------------------------------------------------------------

-- | Transforms a variable into a string representation.
showVarIdx :: VarIdx -> String
showVarIdx v | v >= 0    = if q == 0 then [c] else c : show q
             | otherwise = error err
  where
    (q, r) = divMod v 26
    c = ['a'..'z'] !! r
    err = "Variables can not be represented by negative integers!"

instance Show f => Show (Term f a) where
  show = showTerm

-- | Transforms a term into a string representation.
showTerm :: Show f => Term f a -> String
showTerm = showTerm' False
  where
    showTerm' _ (TermVar _ v)     = showVarIdx v
    showTerm' b (TermCons _ c ts)
      = case ts of
          []     -> show c
          [l, r] -> parensIf b (showTerm' True l ++ " "
                                                 ++ show c
                                                 ++ " "
                                                 ++ showTerm' True r)
          _      -> show c ++ "("
                           ++ intercalate "," (map (showTerm' False) ts)
                           ++ ")"

-- | Transforms a term equation into a string representation.
showTermEq :: Show f => TermEq f a -> String
showTermEq (l, r) = show l ++ " = " ++ show r

-- | Transforms a list of term equations into a string representation.
showTermEqs :: Show f => TermEqs f a -> String
showTermEqs = unlines . map showTermEq