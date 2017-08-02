{-# LANGUAGE FlexibleContexts      #-}
module TypeInference.AHAddVariables (addFreeVariablesInProg) where

import           TypeInference.AbstractHaskell as AH
import           TypeInference.AHAbstract
import           Control.Monad.State.Lazy
import           Data.Map.Lazy                 as DML
import           TypeInference.HSEConversion

-- | Inserts free variales into the actual LState when needed
insertFrees :: MonadState LState m => String -> [VarName] -> m ()
insertFrees name xs = do
  ldb <- get
  let frees' = insert name xs (frees ldb)
  let localsn = insert name (length xs) $ locals ldb
  let localsy = \ x ->  insert name (x + length xs) (locals ldb)
  case DML.lookup name (locals ldb) of
    Nothing -> put LState {frees = frees', locals = localsn}
    Just x  -> put LState {frees = frees', locals = localsy x}

-- | Returns the arity of a function with free variables as parameters
countNewArity :: MonadState LState m => String -> Arity -> m Arity
countNewArity name arity =
 do
   frees <- getCountOfFrees name
   return $ arity + frees

-- | Returns the count of free variables in a function
getCountOfFrees :: MonadState LState m => String -> m Int
getCountOfFrees name = do
 lds <- get
 case DML.lookup name (locals lds) of
   Nothing -> return 0
   Just x  -> return x

-- | Adds a variable to a list of patterns
addToPatterns :: [VarName] -> [Pattern l] -> l -> [Pattern l]
addToPatterns xs ys l =
 Prelude.foldl (\ys x -> ys ++ [AH.PVar NoTypeAnn (x, l)])
               ys
               xs

-------------------------------------------------------------------------------
-- ADDING FREE VARIABLES ------------------------------------------------------
-------------------------------------------------------------------------------

-- | Adds the found free variables out of the local definitions in a
--   program
addFreeVariablesInProg :: MonadState LState m => Prog l -> m (Prog l)
addFreeVariablesInProg (Prog n x y fundecls) =
  do
    newFuncDecls <- mapM addFreeVariablesInFuncDecls fundecls
    return $ Prog n x y newFuncDecls

-- | Adds the found free variables out of the local definitions in a
--   functiondeclaration
addFreeVariablesInFuncDecls ::
  MonadState LState m => FuncDecl l -> m (FuncDecl l)
addFreeVariablesInFuncDecls (Func x y z a b rules) = do
  newRules <- addFreeVariablesInRules rules
  return $ Func x y z a b newRules

-- | Adds the found free variables out of the local definitions in rules
addFreeVariablesInRules :: MonadState LState m => Rules l -> m (Rules l)
addFreeVariablesInRules (Rules rule) = do
  newRule <- mapM addFreeVariablesInRule rule
  return $ Rules newRule

-- | Adds the found free variables out of the local definitions in a rule
addFreeVariablesInRule :: MonadState LState m => AH.Rule l -> m (AH.Rule l)
addFreeVariablesInRule (AH.Rule a b c d e) =
  do
    newRhs <- addFreeVariablesInRhs d
    newLocals <- mapM addFreeVariablesInLocals e
    return $ AH.Rule a b c newRhs newLocals

-- | Adds the found free variables out of the local definitions in a
--   right hand side
addFreeVariablesInRhs :: MonadState LState  m => AH.Rhs l -> m(AH.Rhs l)
addFreeVariablesInRhs (SimpleRhs expr) =
  do
    newExpr <- addFreeVariablesInExpr expr
    return $ SimpleRhs newExpr
addFreeVariablesInRhs (AH.GuardedRhs a exprs) =
  do
    newExprList <- mapM addFreeVariablesInTupel exprs
    return $ AH.GuardedRhs a newExprList

-- | Adds the found free variables out of local definitions in a tupel of expr
addFreeVariablesInTupel ::
  MonadState LState m => (Expr a, Expr a) -> m(Expr a, Expr a)
addFreeVariablesInTupel (a,b) =
  do
    newA <- addFreeVariablesInExpr a
    newB <- addFreeVariablesInExpr b
    return (newA , newB)

-- | Adds the found free variables out of local definitions in an expr
addFreeVariablesInExpr :: MonadState LState m => Expr l -> m (Expr l)
addFreeVariablesInExpr x@(AH.Var _ _)                          =
  return x
addFreeVariablesInExpr x@(AH.Lit _ _)                          =
  return x
addFreeVariablesInExpr x@(AH.Symbol _ _)                       =
  return x
addFreeVariablesInExpr (Apply a tyanno expr1 expr2)            =
  do
    newExpr1 <- addFreeVariablesInExpr expr1
    newExpr2 <- addFreeVariablesInExpr expr2
    return $ Apply a tyanno newExpr1 newExpr2
addFreeVariablesInExpr (InfixApply a tyanno expr1 name expr2)  =
  do
    newExpr1 <- addFreeVariablesInExpr expr1
    newExpr2 <- addFreeVariablesInExpr expr2
    return $ InfixApply a tyanno newExpr1 name newExpr2
addFreeVariablesInExpr (AH.Case a tyanno expr bexprs)          =
  do
    newExpr <- addFreeVariablesInExpr expr
    newBExprs <- mapM addFreeVariablesInBExprs bexprs
    return $ AH.Case a tyanno expr bexprs
addFreeVariablesInExpr (Typed a tyanno expr texpr)             =
  do
    newExpr <- addFreeVariablesInExpr expr
    return $ Typed a tyanno newExpr texpr
addFreeVariablesInExpr (IfThenElse a tyanno expr1 expr2 expr3) =
  do
    newExpr1 <- addFreeVariablesInExpr expr1
    newExpr2 <- addFreeVariablesInExpr expr2
    newExpr3 <- addFreeVariablesInExpr expr3
    return $ IfThenElse a tyanno newExpr1 newExpr2 newExpr3
addFreeVariablesInExpr (AH.Tuple a tyanno exprs)               =
  do
    newExprs <- mapM addFreeVariablesInExpr exprs
    return $ AH.Tuple a tyanno exprs
addFreeVariablesInExpr (AH.List a tyanno exprs)                =
  do
    newExprs <- mapM addFreeVariablesInExpr exprs
    return $ AH.List a tyanno exprs
addFreeVariablesInExpr (AH.Lambda a tyanno pats expr)          =
  do
    newExpr <- addFreeVariablesInExpr expr
    return $ AH.Lambda a tyanno pats expr
addFreeVariablesInExpr (AH.Let a tyanno locals expr)           =
  do
    newExpr <- addFreeVariablesInExpr expr
    newLocals <- mapM addFreeVariablesInLocals locals
    return $ AH.Let a tyanno newLocals newExpr
addFreeVariablesInExpr (DoExpr a tyanno stmts)                 =
  do
    newStmts <- mapM addFreeVariablesInStmt stmts
    return $ DoExpr a tyanno stmts
addFreeVariablesInExpr (AH.ListComp a tyanno expr stmts)       =
  do
    newExpr <- addFreeVariablesInExpr expr
    newStmts <- mapM addFreeVariablesInStmt stmts
    return $ AH.ListComp a tyanno newExpr newStmts

-- | Adds the found free variables out of local definitions in a statement
addFreeVariablesInStmt :: MonadState LState m => Statement l -> m (Statement l)
addFreeVariablesInStmt (SExpr expr)      =
  do
    newExpr <- addFreeVariablesInExpr expr
    return $ SExpr newExpr
addFreeVariablesInStmt (SPat a pat expr) =
  do
  newExpr <- addFreeVariablesInExpr expr
  return $ SPat a pat newExpr
addFreeVariablesInStmt (SLet a locals)   =
  do
    newLocals <- mapM addFreeVariablesInLocals locals
    return $ SLet a newLocals

-- | Adds the found free variables out of local definitions in a branchexpr
addFreeVariablesInBExprs ::
  MonadState LState m => BranchExpr l -> m (BranchExpr l)
addFreeVariablesInBExprs (Branch a pat expr) =
  do
    newExpr <- addFreeVariablesInExpr expr
    return $ Branch a pat newExpr

-- | Adds the found free variables out of local definitions in a local
--   definition
addFreeVariablesInLocals ::
  MonadState LState m => LocalDecl l -> m (LocalDecl l)
addFreeVariablesInLocals (LocalFunc funcdecl) =
  do
    newfunc <- addFreeVariablesAsParametersForFuncDecl funcdecl
    return newfunc
addFreeVariablesInLocals (LocalPat a pat expr locals) =
  do
    newLocals <- mapM addFreeVariablesInLocals locals
    newExpr <- addFreeVariablesInExpr expr
    return $ LocalPat a pat newExpr newLocals

-- | Adds the found free variables out of local definitions in a local
--   functiondefinition
addFreeVariablesAsParametersForFuncDecl ::
  MonadState LState m => FuncDecl a -> m (LocalDecl a)
addFreeVariablesAsParametersForFuncDecl (Func a name arity _ t rules) =
  do
   newRules <- extractFreeVariables (snd $ fst name) rules
   newArity <- countNewArity (snd $ fst name) arity
   return $ LocalFunc $ Func a name newArity Public t newRules

 -- | Adds the found free variables out of local definitions in a pattern

addFreeVariablesAsParametersForPattern :: LocalDecl a -> [LocalDecl a]
addFreeVariablesAsParametersForPattern x@(LocalPat l pat expr locals) = do
  let ev  =  extractFreeVariablesExpr expr
  let f = addToPatterns ev [pat] l
  let t = AH.Rule l NoTypeAnn f (SimpleRhs expr) locals
  let r = Rules [t]
  let pn = parseNamePattern pat
  case length(ev) of
    0 -> return x
    _ -> return $ LocalFunc $ Func l (pn,l) (length ev) Public Untyped r

-------------------------------------------------------------------------------
-- FINDING FREE VARIABLES -----------------------------------------------------
-------------------------------------------------------------------------------

-- | Extracts the free Variables out of Rules
extractFreeVariables :: MonadState LState m => String -> Rules l -> m (Rules l)
extractFreeVariables name (Rules rls) = do
  rules <- mapM (extractFreeVariablesRules name) rls
  return $ Rules rules

-- | Extracts the free variables out of a rule
extractFreeVariablesRules ::
  MonadState LState m => String -> AH.Rule l ->  m (AH.Rule l)
extractFreeVariablesRules name (AH.Rule l t pats rhs locals) = do
   let varExtrleft = extractVars pats
   let varExtrright = extractRhsVars rhs
   let varsToAdd = findDifference varExtrleft varExtrright
   insertFrees name varsToAdd
   let va = addToPatterns (reverse varsToAdd) pats l
   let (_:js)=va
   return $ AH.Rule l t va rhs locals

-- | Extracts the free variables out of a expr
extractFreeVariablesExpr :: Expr l -> [VarName]
extractFreeVariablesExpr (AH.Lambda l t pats expr) =
   findDifference (extractVars pats)(extractFreeVariablesExpr expr)
extractFreeVariablesExpr x = extractVarOutOfExpr x

-------------------------------------------------------------------------------
-- EXTRACTION OF FREE VARIABLES -----------------------------------------------
-------------------------------------------------------------------------------

-- | To later to be able to compare pattern- and exprvariables, just the names
--   of the variables are collected

-- | Extracts the variables out of patterns
extractVars :: [Pattern l] -> [VarName]
extractVars []                         =
  []
extractVars (x@(AH.PVar t (v,l)):xs)   =
  v : extractVars xs
extractVars (PComb l t name pats : xs) =
  extractVars pats ++ extractVars xs
extractVars (PAs l t name pat : xs)    =
  extractVars [pat] ++ extractVars xs
extractVars (AH.PTuple l t pat : xs)   =
  extractVars pat ++ extractVars xs
extractVars (AH.PList l t pat : xs)    =
  extractVars pat ++ extractVars xs
extractVars (x:xs)                     =
  extractVars xs

-- | Extracts the variables out of a right hand side
extractRhsVars :: AH.Rhs l -> [VarName]
extractRhsVars (SimpleRhs expr)        =
  extractVarOutOfExpr expr
extractRhsVars (AH.GuardedRhs _ exprs) =
  extractVarOutOfTups exprs

-- | Extracts the variables out of an expr
extractVarOutOfExpr :: Expr l -> [VarName]
extractVarOutOfExpr (AH.Var t (v,l))                   =
  [v]
extractVarOutOfExpr (Apply l t expr1 expr2)            =
  extractVarOutOfExpr expr1 ++ extractVarOutOfExpr expr2
extractVarOutOfExpr (InfixApply l t expr1 n expr2)     =
  extractVarOutOfExpr expr1 ++ extractVarOutOfExpr expr2
extractVarOutOfExpr (AH.Lambda l t pats expr)          =
  extractVars pats ++ extractVarOutOfExpr expr
extractVarOutOfExpr (AH.Let l t locals expr)           =
  extractVarOutOfExpr expr ++ extractVarsOutOfLocals locals
extractVarOutOfExpr (IfThenElse l t expr1 expr2 expr3) =
  extractVarOutOfExpr expr1 ++ extractVarOutOfExpr expr2 ++ extractVarOutOfExpr expr3
extractVarOutOfExpr (AH.List l t exprs)                =
  concatMap extractVarOutOfExpr exprs
extractVarOutOfExpr (AH.Tuple l t exprs)               =
  concatMap extractVarOutOfExpr exprs
extractVarOutOfExpr (Typed l t expr te)                =
  extractVarOutOfExpr expr
extractVarOutOfExpr (AH.Case l t expr branches)        =
  extractVarOutOfExpr expr ++ concatMap extractVarOutOfBranches branches
extractVarOutOfExpr (DoExpr l t stmts)                 =
  extractVarOutOfStmts stmts
extractVarOutOfExpr (AH.ListComp l t expr1 stm)        =
  extractVarOutOfExpr expr1 ++ extractVarOutOfStmts stm
extractVarOutOfExpr _                                  =
  []

-- | Extracts the variables out of a statement
extractVarOutOfStmts :: [Statement l] -> [VarName]
extractVarOutOfStmts []                     =
  []
extractVarOutOfStmts (SExpr expr : xs)      =
  extractVarOutOfExpr expr ++ extractVarOutOfStmts xs
extractVarOutOfStmts (SPat l pat expr : xs) =
  extractVars [pat] ++ extractVarOutOfExpr expr ++ extractVarOutOfStmts xs
extractVarOutOfStmts (SLet l locals : xs)   =
  extractVarsOutOfLocals locals

-- | Extracts the variables out of local declarations
extractVarsOutOfLocals :: [LocalDecl l] -> [VarName]
extractVarsOutOfLocals []                                =
  []
extractVarsOutOfLocals (LocalFunc funcdecl : xs)         =
  extractVarsOutOfFuncDecl funcdecl ++ extractVarsOutOfLocals xs
extractVarsOutOfLocals (LocalPat a pat expr locals : xs) =
  extractVarOutOfExpr expr ++ extractVarsOutOfLocals locals ++ extractVarsOutOfLocals xs

-- | Extracs the variables out of a function declaration
extractVarsOutOfFuncDecl :: FuncDecl l -> [VarName]
extractVarsOutOfFuncDecl (Func a name arity visibility tsig rules) =
  extractVarOutOfRules rules

-- | Extracts the variables out of rules
extractVarOutOfRules :: Rules l -> [VarName]
extractVarOutOfRules (Rules xs) =
  extractVarOutOfRule xs

-- | Extracts the variables out of a list of rule
extractVarOutOfRule :: [AH.Rule l] -> [VarName]
extractVarOutOfRule [] = []
extractVarOutOfRule (AH.Rule a tyanno pats rhs locals : xs) =
  extractVarOutOfRhs rhs ++
  extractVarsOutOfLocals locals ++ extractVarOutOfRule xs

-- | Extracts the variables out of a right hand side
extractVarOutOfRhs :: AH.Rhs l -> [VarName]
extractVarOutOfRhs (SimpleRhs expr) = extractVarOutOfExpr expr
extractVarOutOfRhs (AH.GuardedRhs a es) = extractVarOutOfTups es

-- | Extracts the variables out of a tupel
extractVarOutOfTups :: [(Expr l, Expr l)] -> [VarName]
extractVarOutOfTups []     =
  []
extractVarOutOfTups (e:es) =
  extractVarOutOfExpr (fst e) ++
  extractVarOutOfExpr (snd e) ++
  extractVarOutOfTups es

-- | Extracts the variables out of a branchexpr
extractVarOutOfBranches :: BranchExpr l -> [VarName]
extractVarOutOfBranches (Branch l pat expr) =
  extractVars [pat] ++ extractVarOutOfExpr expr
