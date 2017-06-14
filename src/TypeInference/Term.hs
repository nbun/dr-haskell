module TypeInference.Term
  ( VarIdx, Term (..), TermEq, TermEqs
  , showVarIdx, showTermEq, showTermEqs, showTermSS
  ) where

import Data.List (intercalate)
import Language.Haskell.Exts.SrcLoc (SrcSpan (..))

-- -----------------------------------------------------------------------------
-- Representation of first-order terms and term equations
-- -----------------------------------------------------------------------------

-- A variable represented as an integer greater than or equal to zero.
type VarIdx = Int

-- Representation of a first-order term, parameterized over the kind of function
-- symbols, e.g., strings.
data Term f = TermVar SrcSpan VarIdx
            | TermCons SrcSpan f [Term f]

-- A term equation represented as a pair of terms and parameterized over the
-- kind of function symbols, e.g., strings.
type TermEq f = (Term f, Term f)

-- Multiple term equations represented as a list of term equations and
-- parameterized over the kind of function symbols, e.g., strings.
type TermEqs f = [TermEq f]

-- -----------------------------------------------------------------------------
-- Pretty-printing of first-order terms and term equations
-- -----------------------------------------------------------------------------

-- Transforms a variable into a string representation.
showVarIdx :: VarIdx -> String
showVarIdx v | v >= 0    = if q == 0 then [c] else c:(show q)
             | otherwise = ""
  where
    (q, r) = divMod v 26
    c = "abcdefghijklmnopqrstuvwxyz" !! r

-- Transforms a term into a string representation.
showTerm :: Show f => Term f -> String
showTerm = showTerm' False
  where
    showTerm' _ (TermVar _ v)     = showVarIdx v
    showTerm' b (TermCons _ c ts)
      = case ts of
          []     -> show c
          [l, r] -> parensIf b ((showTerm' True l) ++ " "
                                                   ++ (show c)
                                                   ++ " "
                                                   ++ (showTerm' True r))
          _      -> (show c) ++ "("
                             ++ (intercalate "," (map (showTerm' False) ts))
                             ++ ")"

-- Transforms a term equation into a string representation.
showTermEq :: Show f => TermEq f -> String
showTermEq (l, r) = (show l) ++ " = " ++ (show r)

-- Transforms a list of term equations into a string representation.
showTermEqs :: Show f => TermEqs f -> String
showTermEqs = unlines . (map showTermEq)

-- Transforms the source span information of a term into a string
-- representation.
showTermSS :: SrcSpan -> String
showTermSS (SrcSpan fn sl sc el ec) = fn ++ ":" ++ (show sl)
                                         ++ ":" ++ (show sc)
                                         ++ ":-:" ++ (show el)
                                         ++ ":" ++ (show ec)

instance Show f => Show (Term f) where
  show = showTerm

-- -----------------------------------------------------------------------------
-- Definition of helper functions
-- -----------------------------------------------------------------------------

-- Encloses a string in parenthesis if the given condition is true.
parensIf :: Bool -> String -> String
parensIf b s = if b then "(" ++ s ++ ")" else s