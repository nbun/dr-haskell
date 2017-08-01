{-|
  Library for normalization of type variables.
-}

module TypeInference.Normalization
  ( Normalization
  , normalize, normTypeExpr, normTypeSig, normTypeAnn, normFuncDecl, normRules
  , normRule, normRhs, normLocalDecl, normExpr, normStatement, normPattern
  , normBranchExpr
  ) where

import           Control.Monad.State                  (State, evalState, get,
                                                       put)
import qualified Data.Map                             as DM
import           Goodies                              (bothM)
import           TypeInference.AbstractHaskell
import           TypeInference.AbstractHaskellGoodies (teVar)

-- -----------------------------------------------------------------------------
-- Representation of type variable normalizations
-- -----------------------------------------------------------------------------

-- | A normalization of type variables represented as a function that takes a
--   term, where the type variables should be normalized, and returns a state
--   consisting of a tuple with the next free variable and a mapping between
--   old and new variables.
type Normalization a = a -> State (Int, DM.Map Int Int) a

-- -----------------------------------------------------------------------------
-- Functions for normalization of type variables
-- -----------------------------------------------------------------------------

-- | Normalizes the given term with the given normalization function.
normalize :: Normalization a -> a -> a
normalize n x = evalState (n x) (0, DM.empty)

-- | The normalization function for type expressions.
normTypeExpr :: Normalization (TypeExpr a)
normTypeExpr (TVar ((v, _), x))
  = do (n, sub) <- get
       case DM.lookup v sub of
         Nothing -> do put (n + 1, DM.insert v n sub)
                       return (teVar n x)
         Just i  -> return (teVar i x)
normTypeExpr (FuncType x t1 t2) = do t1' <- normTypeExpr t1
                                     t2' <- normTypeExpr t2
                                     return (FuncType x t1' t2')
normTypeExpr (TCons x qn tes)   = do tes' <- mapM normTypeExpr tes
                                     return (TCons x qn tes')

-- | The normalization function for type signatures.
normTypeSig :: Normalization (TypeSig a)
normTypeSig Untyped      = return Untyped
normTypeSig (TypeSig te) = do te' <- normTypeExpr te
                              return (TypeSig te')

-- | The normalization function for type annotations.
normTypeAnn :: Normalization (TypeAnn a)
normTypeAnn NoTypeAnn    = return NoTypeAnn
normTypeAnn (TypeAnn te) = do te' <- normTypeExpr te
                              return (TypeAnn te')

-- | The normalization function for function declarations.
normFuncDecl :: Normalization (FuncDecl a)
normFuncDecl (Func x qn a v ts rs) = do ts' <- normTypeSig ts
                                        rs' <- normRules rs
                                        return (Func x qn a v ts' rs')

-- | The normalization function for rules declarations.
normRules :: Normalization (Rules a)
normRules (Rules rs)      = do rs' <- mapM normRule rs
                               return (Rules rs')
normRules (External x ta) = do ta' <- normTypeAnn ta
                               return (External x ta')

-- | The normalization function for function rules.
normRule :: Normalization (Rule a)
normRule (Rule x ta ps rhs lds) = do ta' <- normTypeAnn ta
                                     ps' <- mapM normPattern ps
                                     rhs' <- normRhs rhs
                                     lds' <- mapM normLocalDecl lds
                                     return (Rule x ta' ps' rhs' lds')

-- | The normalization function for right-hand sides.
normRhs :: Normalization (Rhs a)
normRhs (SimpleRhs e)      = do e' <- normExpr e
                                return (SimpleRhs e')
normRhs (GuardedRhs x eqs) = do eqs' <- mapM (bothM normExpr) eqs
                                return (GuardedRhs x eqs')

-- | The normalization function for local declarations.
normLocalDecl :: Normalization (LocalDecl a)
normLocalDecl (LocalFunc fd)       = do fd' <- normFuncDecl fd
                                        return (LocalFunc fd')
normLocalDecl (LocalPat x p e lds) = do p' <- normPattern p
                                        e' <- normExpr e
                                        lds' <- mapM normLocalDecl lds
                                        return (LocalPat x p' e' lds')

-- | The normalization function for expressions.
normExpr :: Normalization (Expr a)
normExpr (Var ta vn)                = do ta' <- normTypeAnn ta
                                         return (Var ta' vn)
normExpr (Lit ta l)                 = do ta' <- normTypeAnn ta
                                         return (Lit ta' l)
normExpr (Symbol ta qn)             = do ta' <- normTypeAnn ta
                                         return (Symbol ta' qn)
normExpr (Apply x ta e1 e2)         = do ta' <- normTypeAnn ta
                                         e1' <- normExpr e1
                                         e2' <- normExpr e2
                                         return (Apply x ta' e1' e2')
normExpr (InfixApply x ta e1 qn e2) = do ta' <- normTypeAnn ta
                                         e1' <- normExpr e1
                                         e2' <- normExpr e2
                                         return (InfixApply x ta' e1' qn e2')
normExpr (Lambda x ta ps e)         = do ta' <- normTypeAnn ta
                                         ps' <- mapM normPattern ps
                                         e' <- normExpr e
                                         return (Lambda x ta' ps' e')
normExpr (Let x ta lds e)           = do ta' <- normTypeAnn ta
                                         lds' <- mapM normLocalDecl lds
                                         e' <- normExpr e
                                         return (Let x ta' lds' e')
normExpr (DoExpr x ta sts)          = do ta' <- normTypeAnn ta
                                         sts' <- mapM normStatement sts
                                         return (DoExpr x ta' sts')
normExpr (ListComp x ta e sts)      = do ta' <- normTypeAnn ta
                                         e' <- normExpr e
                                         sts' <- mapM normStatement sts
                                         return (ListComp x ta' e' sts')
normExpr (Case x ta e bs)           = do ta' <- normTypeAnn ta
                                         e' <- normExpr e
                                         bs' <- mapM normBranchExpr bs
                                         return (Case x ta' e' bs')
normExpr (Typed x ta e te)          = do ta' <- normTypeAnn ta
                                         e' <- normExpr e
                                         te' <- normTypeExpr te
                                         return (Typed x ta' e' te')
normExpr (IfThenElse x ta e1 e2 e3) = do ta' <- normTypeAnn ta
                                         e1' <- normExpr e1
                                         e2' <- normExpr e2
                                         e3' <- normExpr e3
                                         return (IfThenElse x ta' e1' e2' e3')
normExpr (Tuple x ta es)            = do ta' <- normTypeAnn ta
                                         es' <- mapM normExpr es
                                         return (Tuple x ta' es')
normExpr (List x ta es)             = do ta' <- normTypeAnn ta
                                         es' <- mapM normExpr es
                                         return (List x ta' es')

-- | The normalization function for branch expressions.
normBranchExpr :: Normalization (BranchExpr a)
normBranchExpr (Branch x p e) = do p' <- normPattern p
                                   e' <- normExpr e
                                   return (Branch x p' e')

-- | The normalization function for patterns.
normPattern :: Normalization (Pattern a)
normPattern (PVar ta vn)       = do ta' <- normTypeAnn ta
                                    return (PVar ta' vn)
normPattern (PLit ta l)        = do ta' <- normTypeAnn ta
                                    return (PLit ta' l)
normPattern (PComb x ta qn ps) = do ta' <- normTypeAnn ta
                                    ps' <- mapM normPattern ps
                                    return (PComb x ta' qn ps')
normPattern (PAs x ta vn p)    = do ta' <- normTypeAnn ta
                                    p' <- normPattern p
                                    return (PAs x ta' vn p')
normPattern (PTuple x ta ps)   = do ta' <- normTypeAnn ta
                                    ps' <- mapM normPattern ps
                                    return (PTuple x ta' ps')
normPattern (PList x ta ps)    = do ta' <- normTypeAnn ta
                                    ps' <- mapM normPattern ps
                                    return (PList x ta' ps')

-- | The normalization function for statements.
normStatement :: Normalization (Statement a)
normStatement (SExpr e)    = do e' <- normExpr e
                                return (SExpr e')
normStatement (SPat x p e) = do p' <- normPattern p
                                e' <- normExpr e
                                return (SPat x p' e')
normStatement (SLet x lds) = do lds' <- mapM normLocalDecl lds
                                return (SLet x lds')
