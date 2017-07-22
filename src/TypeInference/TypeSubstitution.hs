{-|
  Library for representation of substitutions on type expressions.
-}

module TypeInference.TypeSubstitution
  ( TESubst
  , showTESubst, emptyTESubst, extendTESubst, listToTESubst, teSubstToList
  , lookupTESubst, applyTESubst, applyTESubstTS, applyTESubstTA, applyTESubstFD
  , applyTESubstRS, applyTESubstR, applyTESubstRHS, applyTESubstLD
  , applyTESubstE, applyTESubstS, applyTESubstP, applyTESubstBE, restrictTESubst
  , composeTESubst
  ) where

import Data.List (intercalate)
import qualified Data.Map as DM
import Data.Maybe (fromMaybe)
import Goodies (both)
import TypeInference.AbstractHaskell

-- -----------------------------------------------------------------------------
-- Representation of substitutions on type expressions
-- -----------------------------------------------------------------------------

-- | A substitution represented as a map from variables to type expressions and
--   parameterized over the type of annotations.
type TESubst a = DM.Map Int (TypeExpr a)

-- -----------------------------------------------------------------------------
-- Pretty-printing of substitutions on type expressions
-- -----------------------------------------------------------------------------

-- \8614 = RIGHTWARDS ARROW FROM BAR

-- | Transforms a substitution into a string representation.
showTESubst :: AHOptions -> TESubst a -> String
showTESubst opts sub
  = "{" ++ intercalate "," (map showMapping (teSubstToList sub)) ++ "}"
  where
    showMapping :: (Int, TypeExpr a) -> String
    showMapping (v, te) = varToString v ++ " \8614 " ++ showTypeExpr opts te

-- -----------------------------------------------------------------------------
-- Functions for substitutions on type expressions
-- -----------------------------------------------------------------------------

-- | The empty substitution.
emptyTESubst :: TESubst a
emptyTESubst = DM.empty

-- | Extends a substitution with a new mapping from the given variable to the
--   given type expression. An already existing mapping with the same variable
--   will be thrown away.
extendTESubst :: Int -> TypeExpr a -> TESubst a -> TESubst a
extendTESubst = DM.insert

-- | Returns a substitution that contains all the mappings from the given list.
--   For multiple mappings with the same variable, the last corresponding
--   mapping of the given list is taken.
listToTESubst :: [(Int, TypeExpr a)] -> TESubst a
listToTESubst = DM.fromList

-- | Returns a list with all mappings from the given substitution.
teSubstToList :: TESubst a -> [(Int, TypeExpr a)]
teSubstToList = DM.toList

-- | Returns the type expression mapped to the given variable in a substitution
--   or 'Nothing' if no such mapping exists.
lookupTESubst :: Int -> TESubst a -> Maybe (TypeExpr a)
lookupTESubst = DM.lookup

-- | Applies a substitution to the given type expression.
applyTESubst :: TESubst a -> TypeExpr a -> TypeExpr a
applyTESubst sub t@(TVar ((v, _), _)) = fromMaybe t (lookupTESubst v sub)
applyTESubst sub (TCons x qn tes)     = TCons x qn (map (applyTESubst sub) tes)
applyTESubst sub (FuncType x t1 t2)
  = FuncType x (applyTESubst sub t1) (applyTESubst sub t2)

-- | Applies a substitution to the given type signature.
applyTESubstTS :: TESubst a -> TypeSig a -> TypeSig a
applyTESubstTS sub Untyped      = Untyped
applyTESubstTS sub (TypeSig te) = TypeSig (applyTESubst sub te)

-- | Applies a substitution to the given type annotation.
applyTESubstTA :: TESubst a -> TypeAnn a -> TypeAnn a
applyTESubstTA sub NoTypeAnn    = NoTypeAnn
applyTESubstTA sub (TypeAnn te) = TypeAnn (applyTESubst sub te)

-- | Applies a substitution to the given function declaration.
applyTESubstFD :: TESubst a -> FuncDecl a -> FuncDecl a
applyTESubstFD sub (Func x qn a v ts rs)
  = Func x qn a v (applyTESubstTS sub ts) (applyTESubstRS sub rs)

-- | Applies a substitution to the given rules declaration.
applyTESubstRS :: TESubst a -> Rules a -> Rules a
applyTESubstRS sub (Rules rs)      = Rules (map (applyTESubstR sub) rs)
applyTESubstRS sub (External x ta) = External x (applyTESubstTA sub ta)

-- | Applies a substitution to the given function rule.
applyTESubstR :: TESubst a -> Rule a -> Rule a
applyTESubstR sub (Rule x ta ps rhs lds)
  = Rule x (applyTESubstTA sub ta)
           (map (applyTESubstP sub) ps)
           (applyTESubstRHS sub rhs)
           (map (applyTESubstLD sub) lds)

-- | Applies a substitution to the given right-hand side.
applyTESubstRHS :: TESubst a -> Rhs a -> Rhs a
applyTESubstRHS sub (SimpleRhs e)      = SimpleRhs (applyTESubstE sub e)
applyTESubstRHS sub (GuardedRhs x eqs)
  = GuardedRhs x (map (both (applyTESubstE sub)) eqs)

-- | Applies a substitution to the given local declaration.
applyTESubstLD :: TESubst a -> LocalDecl a -> LocalDecl a
applyTESubstLD sub (LocalFunc fd)       = LocalFunc (applyTESubstFD sub fd)
applyTESubstLD sub (LocalPat x p e lds)
  = LocalPat x (applyTESubstP sub p)
               (applyTESubstE sub e)
               (map (applyTESubstLD sub) lds)

-- | Applies a substitution to the given expression.
applyTESubstE :: TESubst a -> Expr a -> Expr a
applyTESubstE sub (Var ta vn)                = Var (applyTESubstTA sub ta) vn
applyTESubstE sub (Lit ta l)                 = Lit (applyTESubstTA sub ta) l
applyTESubstE sub (Symbol ta qn)             = Symbol (applyTESubstTA sub ta) qn
applyTESubstE sub (Apply x ta e1 e2)
  = Apply x (applyTESubstTA sub ta)
            (applyTESubstE sub e1)
            (applyTESubstE sub e2)
applyTESubstE sub (InfixApply x ta e1 qn e2)
  = InfixApply x (applyTESubstTA sub ta)
                 (applyTESubstE sub e1)
                 qn
                 (applyTESubstE sub e2)
applyTESubstE sub (Lambda x ta ps e)
  = Lambda x (applyTESubstTA sub ta)
             (map (applyTESubstP sub) ps)
             (applyTESubstE sub e)
applyTESubstE sub (Let x ta lds e)
  = Let x (applyTESubstTA sub ta)
          (map (applyTESubstLD sub) lds)
          (applyTESubstE sub e)
applyTESubstE sub (DoExpr x ta sts)
  = DoExpr x (applyTESubstTA sub ta) (map (applyTESubstS sub) sts)
applyTESubstE sub (ListComp x ta e sts)
  = ListComp x (applyTESubstTA sub ta)
               (applyTESubstE sub e)
               (map (applyTESubstS sub) sts)
applyTESubstE sub (Case x ta e bes)
  = Case x (applyTESubstTA sub ta)
           (applyTESubstE sub e)
           (map (applyTESubstBE sub) bes)
applyTESubstE sub (Typed x ta e te)
  = Typed x (applyTESubstTA sub ta) (applyTESubstE sub e) (applyTESubst sub te)
applyTESubstE sub (IfThenElse x ta e1 e2 e3)
  = IfThenElse x (applyTESubstTA sub ta)
                 (applyTESubstE sub e1)
                 (applyTESubstE sub e2)
                 (applyTESubstE sub e3)
applyTESubstE sub (Tuple x ta es)
  = Tuple x (applyTESubstTA sub ta) (map (applyTESubstE sub) es)
applyTESubstE sub (List x ta es)
  = List x (applyTESubstTA sub ta) (map (applyTESubstE sub) es)

-- | Applies a substitution to the given branch expression.
applyTESubstBE :: TESubst a -> BranchExpr a -> BranchExpr a
applyTESubstBE sub (Branch x p e)
  = Branch x (applyTESubstP sub p) (applyTESubstE sub e)

-- | Applies a substitution to the given pattern.
applyTESubstP :: TESubst a -> Pattern a -> Pattern a
applyTESubstP sub (PVar ta vn)       = PVar (applyTESubstTA sub ta) vn
applyTESubstP sub (PLit ta l)        = PLit (applyTESubstTA sub ta) l
applyTESubstP sub (PComb x ta qn ps)
  = PComb x (applyTESubstTA sub ta) qn (map (applyTESubstP sub) ps)
applyTESubstP sub (PAs x ta vn p)
  = PAs x (applyTESubstTA sub ta) vn (applyTESubstP sub p)
applyTESubstP sub (PTuple x ta ps)
  = PTuple x (applyTESubstTA sub ta) (map (applyTESubstP sub) ps)
applyTESubstP sub (PList x ta ps)
  = PList x (applyTESubstTA sub ta) (map (applyTESubstP sub) ps)

-- | Applies a substitution to the given statement.
applyTESubstS :: TESubst a -> Statement a -> Statement a
applyTESubstS sub (SExpr e)    = SExpr (applyTESubstE sub e)
applyTESubstS sub (SPat x p e)
  = SPat x (applyTESubstP sub p) (applyTESubstE sub e)
applyTESubstS sub (SLet x lds) = SLet x (map (applyTESubstLD sub) lds)

-- | Returns a new substitution with only those mappings from the given
--   substitution whose variable is in the given list of variables.
restrictTESubst :: TESubst a -> [Int] -> TESubst a
restrictTESubst sub vs
  = listToTESubst [(v, te) | v <- vs, Just te <- [lookupTESubst v sub]]

-- | Composes the first substitution @phi@ with the second substitution @sigma@.
--   The resulting substitution @sub@ fulfills the property
--   @sub(t) = phi(sigma(t))@ for a term @t@. Mappings in the first substitution
--   shadow those in the second.
composeTESubst :: TESubst a -> TESubst a -> TESubst a
composeTESubst phi sigma = DM.union phi (DM.map (applyTESubst phi) sigma)