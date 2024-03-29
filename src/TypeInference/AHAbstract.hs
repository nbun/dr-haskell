{-# LANGUAGE FlexibleContexts      #-}

module TypeInference.AHAbstract (abstrProg,LState(..),initialStateLambda) where

import           Language.Haskell.Exts as HSE
import           TypeInference.AbstractHaskell  as AH
import           Control.Monad.State.Lazy
import           TypeInference.TypeSig
import           Data.Map.Lazy                  as DML


-- | State for lambda lifting
--   locals contains count of free variables for the functions
--   frees contains the names of the free variables for a function
data LState  = LState { locals :: Map String Int
                      , frees  :: Map String [VarName]
                      }

-- | Initialstate for lambda lifting
initialStateLambda = LState empty empty
-------------------------------------------------------------------------------
-- ABSTRACT REPRESENTATION ----------------------------------------------------
-------------------------------------------------------------------------------

-- | Builds the abstract representation of a abstract haskell program
abstrProg :: MonadState LState m => Prog a -> m (Prog a)
abstrProg (Prog name imps types fundecls) =
  do
    funDefN <- mapM (abstrFuncDef "") fundecls
    return $ Prog name imps types funDefN

-- | Builds the abstract representation of a function name (as qname)
abstrFuncName ::
  MonadState LState m => String -> (AH.QName,a) -> m (AH.QName,a)
abstrFuncName name fname =
  do
    ldb <- get
    let funName =  snd $ fst fname
    let funId =  fst $ fst fname
    case DML.lookup funName (frees ldb) of
      Nothing -> return fname
      Just y -> return ((funId, name ++ "." ++ funName), snd fname)

-- | Builds the abstract representation of a function declaration
abstrFuncDef ::
  MonadState LState m => String -> FuncDecl a -> m (FuncDecl a)
abstrFuncDef name (Func a fname arity visibility tsig rules)  =
  do
    newName <- abstrFuncName name fname
    rulesN <- abstrRules (snd $ fst fname) rules
    return $ Func a newName arity visibility tsig rulesN

-- | Builds the abstract representation of Rules
abstrRules :: MonadState LState m => String -> Rules a -> m (Rules a)
abstrRules name (Rules rules) =
  do
    newRules <- mapM (abstrRule name) rules
    return $ Rules newRules

-- | Builds the abstract representation of a rule
abstrRule :: MonadState LState m => String -> AH.Rule a -> m (AH.Rule a)
abstrRule name (AH.Rule a tanno pats rhs localdecls) = do
   rhside <- abstrRhs name rhs
   ldcls <- mapM (abstrLocal name) localdecls
   return $ AH.Rule a tanno pats rhside ldcls

-- | Builds the abstract representation for a rigth hand side
abstrRhs :: MonadState LState m => String -> AH.Rhs a -> m (AH.Rhs a)
abstrRhs name (SimpleRhs expr) = do
  exprN <- abstrExpr name expr
  return  $ SimpleRhs exprN
abstrRhs name (AH.GuardedRhs a exprsTups) = do
  newTups <- mapM (abstrTupel name) exprsTups
  return $ AH.GuardedRhs a newTups

-- | Builds the abstract representation of a tupel ((expr, expr))
abstrTupel ::
  MonadState LState m => String -> (Expr a, Expr a1) -> m (Expr a, Expr a1)
abstrTupel name (expr1,expr2) =
  do
    exprN1 <- abstrExpr name expr1
    exprN2 <- abstrExpr name expr2
    return (exprN1,exprN2)

-- | Builds the abstract representation of a local declaration
abstrLocal :: MonadState LState m => String -> LocalDecl a -> m (LocalDecl a)
abstrLocal name (LocalFunc (Func a qn ar v t rls)) =
  do
    newName <- abstrFuncName name qn
    rulesN <- abstrRules name rls
    return $ LocalFunc (Func a newName ar v t rulesN)
abstrLocal name x@(LocalPat a pat expr locals) = do
  --exprN <- abstrExpr name expr
  --localsN <- mapM (abstrLocal name) locals
  return $ x--LocalPat a pat exprN localsN

-- | Builds the abstract representation of an expr
abstrExpr :: MonadState LState m => String -> Expr a -> m (Expr a)
abstrExpr name x@(AH.Var tanno vname)                                     =
  return x
abstrExpr name x@(AH.Lit tanno lit)                                       =
  return x
abstrExpr name x@(AH.Symbol tyanno qname)                                 =
  do
    lds <- get
    let realName = snd $ fst qname
    let newName = name ++ "." ++ realName
    case DML.lookup realName (frees lds) of
      Nothing ->  return x
      Just y ->
         return $ AH.Symbol tyanno ((fst $ fst qname , newName),snd qname)
abstrExpr name x@(AH.Lambda a tyanno pats expr)                           =
  do
    exprN <- abstrExpr name expr
    return $ AH.Lambda a tyanno pats exprN
abstrExpr name x@(AH.Let a tyanno locals expr)                            =
  do
    newLocals <- mapM (abstrLocal name) locals
    newExpr   <- abstrExpr name expr
    return $ AH.Let a tyanno newLocals newExpr
abstrExpr name x@(DoExpr a tyanno stms)                                   =
  do
    stmtN <- mapM (abstrStmt name) stms
    return $  DoExpr a tyanno stmtN
abstrExpr name x@(AH.ListComp a tyanno expr stmts)                        =
  do
    exprN <- abstrExpr name expr
    stmtN <- mapM (abstrStmt name) stmts
    return $AH.ListComp a tyanno exprN stmtN
abstrExpr name x@(AH.Case a tyanno expr bes)                              =
  do
    exprN <- abstrExpr name expr
    bexp <- mapM (abstrBranch name) bes
    return $ AH.Case a tyanno exprN bexp
abstrExpr name x@(Typed a tyanno expr texpr)                              =
  do
    exprN <- abstrExpr name expr
    return $ Typed a tyanno exprN texpr
abstrExpr name x@(IfThenElse a tyanno expr1 expr2 expr3)                  =
  do
    exprN1 <- abstrExpr name expr1
    exprN2 <- abstrExpr name expr2
    exprN3 <- abstrExpr name expr3
    return $  IfThenElse a tyanno exprN1 exprN2 exprN3
abstrExpr name x@(AH.Tuple a tyanno exprs)                                =
  do
    exprsN <- mapM (abstrExpr name) exprs
    return $ AH.Tuple a tyanno exprsN
abstrExpr name x@(AH.List a tyanno exprs)                                 =
  do
    exprsN <- mapM (abstrExpr name) exprs
    return $ AH.List a tyanno exprsN
abstrExpr name x@(Apply a tyanno z@(AH.Symbol tanno qname) expr2)         =
  do
    lds <- get
    case DML.lookup (snd $ fst qname) (frees lds) of
      Nothing -> do
                 z' <- abstrExpr name z
                 exprN2 <- abstrExpr name expr2
                 return $ Apply a tyanno z' exprN2
      Just y -> do
                  e1n <- abstrExpr name z
                  e2n <- abstrExpr name expr2
                  expr2Ext <- ePara (snd $ fst qname) (Apply a tyanno e1n e2n) a
                  return $ expr2Ext
abstrExpr name x@(Apply a tyanno expr1 expr2)                             =
  do
    exprN1 <- abstrExpr name expr1
    exprN2 <- abstrExpr name expr2
    return $ Apply a tyanno exprN1 exprN2
abstrExpr name x@(InfixApply a ty z@(AH.Symbol tanno qname) qname' expr2) =
  do
    lds <- get
    case DML.lookup (snd $ fst qname) (frees lds) of
      Nothing -> do
                   z' <- abstrExpr name z
                   exprN2 <- abstrExpr name expr2
                   return $ InfixApply a ty z' qname exprN2
      Just y -> do
                  expr2Ext <- ePara (snd $ fst qname) z a
                  ex2 <- abstrExpr name expr2Ext
                  z' <-  abstrExpr name z
                  return $ InfixApply a ty z' qname ex2
abstrExpr name x@ (InfixApply a tyanno expr1 qname expr2)                 =
  do
    exprN1 <- abstrExpr name expr1
    exprN2 <- abstrExpr name expr2
    return $ InfixApply a tyanno exprN1 qname exprN2

-- | Builds the abstract representation of a statement
abstrStmt ::
  MonadState LState m => String -> Statement a -> m (Statement a)
abstrStmt name (SExpr expr)      =
  do
    expr1 <- abstrExpr name expr
    return $ SExpr expr1
abstrStmt name (SPat a pat expr) =
  do
    expr1 <- abstrExpr name expr
    return $ SPat a pat expr1
abstrStmt name (SLet a locals)   =
  do
    newLocals <- mapM (abstrLocal name) locals
    return $ SLet a newLocals

-- | Builds the abstract representation of a branchExpr
abstrBranch ::
  MonadState LState m => String -> BranchExpr a -> m (BranchExpr a)
abstrBranch name (Branch a pat expr) =
  do
    exprN <- abstrExpr name expr
    return $ Branch a pat exprN

-------------------------------------------------------------------------------
-- EXTENSION OF PARAMETERS ----------------------------------------------------
-------------------------------------------------------------------------------

ePara :: MonadState LState m => String -> Expr a -> a -> m (Expr a)
ePara str e1 a=
  do
     lds <- get
     let  values = frees lds ! str
     let eprVars = transformVars a values []
     let newExpr = extendParameters2 a e1 eprVars
     return newExpr

--extendParameters2 :: a -> Expr a -> [Expr a] -> Expr a
extendParameters2 a e1 [] = e1
extendParameters2 a e1 (x:xs) = extendParameters2 a (Apply a NoTypeAnn e1 x) xs

-- | Tranforms variablenames into a variable(-expr) and adds them to a list of
--   exprs
transformVars :: a -> [VarName] -> [Expr a] -> [Expr a]
transformVars a  [] z = z
transformVars a (x:xs) y = transformVars a xs (y ++[AH.Var NoTypeAnn (x, a)] )
