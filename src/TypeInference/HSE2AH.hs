{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypeInference.HSE2AH (hseToAH,parseFile') where

import           Control.Monad.State.Lazy
import           Data.Functor
import           Data.Map.Lazy                  as DML
import           Language.Haskell.Exts          as HSE
import           TypeInference.AbstractHaskell  as AH
import           TypeInference.TypeSig


parseFile' :: FilePath -> IO (Module SrcSpanInfo)
parseFile' f = do
          (ParseOk ast) <- parseFile f
          return ast

-----------------------------------------------------------
-- MAIN FUNCTION --------------------------------------------------------------
-------------------------------------------------------------------------------

-- | Main function to convert HSE modules to abstract Haskell programs.
hseToAH :: Module a -> Prog a
hseToAH modu =  evalState (nlahToAH (hseToNLAH modu)) initialStateLambda

-------------------------------------------------------------------------------
-- LAMBDA LIFTING FOR LOCAL DECLARATIONS  -------------------------------------
-------------------------------------------------------------------------------

-- | State for lambda lifting
--   locals contains count of free variables for the functions
--   frees contains the names of the free variables for a function
data LState  = LState { locals :: Map String Int
                      , frees  :: Map String [VarName]
                      }

-- | Initialstate for lambda lifting
initialStateLambda = LState empty empty

-- | Executes the lambda lifting in three steps
--   1. adds free variables to local functions as parameters
--   2. builds a abstract representations with new names for the localdecls
--      functions
--   3. lifts the local functions on top level
nlahToAH :: MonadState LState m => Prog l -> m (Prog l )
nlahToAH p@(Prog m n t fs) = do
   p1 <- addFreeVariablesInProg p
   v@(Prog n i t fd) <- abstrProg p1
   let list = transFormLocalProg [] v
   let newProg = Prog n i t (fd ++ list)
   return newProg

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
abstrLocal name (LocalFunc funcdecls) = do
  x <- abstrFuncDef name funcdecls
  return $ LocalFunc x
abstrLocal name (LocalPat a pat expr locals) = do
  exprN <- abstrExpr name expr
  localsN <- mapM (abstrLocal name) locals
  return $ LocalPat a pat exprN localsN

-- | Builds the abstract representation of an expr
abstrExpr :: MonadState LState m => String -> Expr a -> m (Expr a)
abstrExpr name x@(AH.Var tanno vname)                                     =
  do
    lds <- get
    let realName = snd $ fst vname
    let newName =  name ++  "." ++ realName
    case DML.lookup realName (frees lds) of
     Nothing -> return x
     Just y -> return $ AH.Var tanno ((fst $ fst vname ,newName), snd vname)
abstrExpr name x@(AH.Lit tanno lit)                                       =
  return x
abstrExpr name x@(AH.Symbol tyanno qname)                                 =
  return x
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
abstrExpr name x@(Apply a tyanno z@(AH.Var tanno vname) expr2)            =
  do
    lds <- get
    case DML.lookup (snd $ fst vname) (frees lds) of
      Nothing -> do
                 z' <- abstrExpr name z
                 exprN2 <- abstrExpr name expr2
                 return $ Apply a tyanno z' exprN2
      Just y -> do
                  expr2Ext <- extendParameters a (snd $ fst vname)  expr2
                  ex2 <- abstrExpr name expr2Ext
                  z' <-  abstrExpr name z
                  return $ Apply a tyanno z' ex2
abstrExpr name x@(Apply a tyanno expr1 expr2)                             =
  do
    exprN1 <- abstrExpr name expr1
    exprN2 <- abstrExpr name expr2
    return $ Apply a tyanno exprN1 exprN2
abstrExpr name x@(InfixApply a tyanno z@(AH.Var tanno vname) qname expr2) =
  do
    lds <- get
    case DML.lookup (snd $ fst vname) (frees lds) of
      Nothing -> do
                   z' <- abstrExpr name z
                   exprN2 <- abstrExpr name expr2
                   return $ InfixApply a tyanno z' qname exprN2
      Just y -> do
                  expr2Ext <- extendParameters a (snd $ fst vname)  expr2
                  ex2 <- abstrExpr name expr2Ext
                  z' <-  abstrExpr name z
                  return $ InfixApply a tyanno z' qname ex2
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

-- | Extends parameters to a exprN
--   for list : add parameter at the end of the list
--   for an single expr : build a list and add fist the expr and then the
--   parameters
extendParameters :: MonadState LState m => a -> String -> Expr a -> m (Expr a)
extendParameters b name x@(AH.List a tyanno exprs) =
  do
    lds <- get
    let  values = frees lds ! name
    let eprVars = transformVars a values exprs
    return $ AH.List a tyanno eprVars
extendParameters a name x                          =
  do
    lds <- get
    let values = frees lds ! name
    let exprVars = transformVars a values [x]
    return $ AH.List a NoTypeAnn exprVars

-- | Tranforms variablenames into a variable(-expr) and adds them to a list of
--   exprs
transformVars :: a -> [VarName] -> [Expr a] -> [Expr a]
transformVars a  [] z = z
transformVars a (x:xs) y = transformVars a xs (y ++[AH.Var NoTypeAnn (x, a)] )

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
    return $ AH.Rule  a b c newRhs newLocals

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

-------------------------------------------------------------------------------
-- LIFTING TO TOPLEVEL --------------------------------------------------------
-------------------------------------------------------------------------------

-- | Lifts all local declarations of a programm to toplevel
transFormLocalProg :: [FuncDecl l] -> Prog l -> [FuncDecl l]
transFormLocalProg list (Prog n x y fundecls) =
  list ++ concatMap (transFormLocalFuncDecl list) fundecls

-- | Lifts all local declarations of the function declarations to toplevel
transFormLocalFuncDecl :: [FuncDecl l] -> FuncDecl l -> [FuncDecl l]
transFormLocalFuncDecl list (Func x y z a b rules) =
  list ++ transFormLocalRules list rules

-- | Lifts all local declarations of rules to toplevel
transFormLocalRules :: [FuncDecl l] -> Rules l -> [FuncDecl l]
transFormLocalRules list (Rules rule) =
  list ++ concatMap (transFormLocalRule list) rule

-- | Lifts all local declarations of a rule to toplevel
transFormLocalRule :: [FuncDecl l] -> AH.Rule l -> [FuncDecl l]
transFormLocalRule list (AH.Rule a b c d e) =
  list ++ transFormLocalRhs list d ++ concatMap transFormLocal e

-- | Lifts all local declarations of a right hand side to toplevel
transFormLocalRhs :: [FuncDecl l] -> AH.Rhs l -> [FuncDecl l]
transFormLocalRhs list (SimpleRhs expr)        =
  list ++ transFormLocalExpr list expr
transFormLocalRhs list (AH.GuardedRhs a exprs) =
  list ++ concatMap (transFormLocalListExpr list) exprs

-- | Lifts all local declarations of a exprtupel to toplevel
transFormLocalListExpr :: [FuncDecl l] -> (Expr l, Expr l) -> [FuncDecl l]
transFormLocalListExpr list (a,b) =
  list ++ transFormLocalExpr list a ++  transFormLocalExpr list b

-- | Lifts all local declarations of an expr to toplevel
transFormLocalExpr :: [FuncDecl l] -> Expr l -> [FuncDecl l]
transFormLocalExpr list x@(AH.Var _ _)                          =
  list
transFormLocalExpr list x@(AH.Lit _ _)                          =
  list
transFormLocalExpr list x@(AH.Symbol _ _)                       =
  list
transFormLocalExpr list (Apply a tyanno expr1 expr2)            =
  list ++ transFormLocalExpr list expr1 ++ transFormLocalExpr list expr2
transFormLocalExpr list (InfixApply a tyanno expr1 name expr2)  =
  list ++ transFormLocalExpr list expr1 ++ transFormLocalExpr list expr2
transFormLocalExpr list (AH.Case a tyanno expr bexprs)          =
  list ++ transFormLocalExpr list expr ++
  concatMap (transFormLocalExprBranches list) bexprs
transFormLocalExpr list (Typed a tyanno expr texpr)             =
  list ++ transFormLocalExpr list expr
transFormLocalExpr list (IfThenElse a tyanno expr1 expr2 expr3) =
  list ++ transFormLocalExpr list expr1 ++
  transFormLocalExpr list expr2 ++
  transFormLocalExpr list expr3
transFormLocalExpr list (AH.Tuple a tyanno exprs)               =
  list ++ concatMap (transFormLocalExpr list) exprs
transFormLocalExpr list (AH.List a tyanno exprs)                =
  list ++ concatMap (transFormLocalExpr list) exprs
transFormLocalExpr list (AH.Lambda a tyanno pats expr)          =
  list ++ transFormLocalExpr list expr
transFormLocalExpr list (AH.Let a tyanno locals expr)           =
  list ++ transFormLocalExpr list expr ++ concatMap transFormLocal locals
transFormLocalExpr list (DoExpr a tyanno stmts)                 =
  list ++ concatMap (transFormLocalStmt list) stmts
transFormLocalExpr list (AH.ListComp a tyanno expr stmts)       =
  list ++ transFormLocalExpr list expr ++
  concatMap (transFormLocalStmt list) stmts

-- | Lifts all local declarations of a statemen to toplevel
transFormLocalStmt :: [FuncDecl l] -> Statement l -> [FuncDecl l]
transFormLocalStmt list (SExpr expr)      =
  list ++ transFormLocalExpr list expr
transFormLocalStmt list (SPat a pat expr) =
  list ++ transFormLocalExpr list expr
transFormLocalStmt list (SLet a locals)   =
  list ++ concatMap transFormLocal locals

-- | Lifts all local declarations of a branchexpr to toplevel
transFormLocalExprBranches :: [FuncDecl l] -> BranchExpr l -> [FuncDecl l]
transFormLocalExprBranches list (Branch a pat expr) =
  list ++ transFormLocalExpr list expr

-- | Lifts all local declarations to toplevel
transFormLocal :: LocalDecl l -> [FuncDecl l]
transFormLocal (LocalFunc (Func a b c Private d e)) = [(Func a b c Public d e)]
transFormLocal (LocalPat l pat expr lcs) =
  [Func l (("",""),l) undefined Public Untyped (Rules [AH.Rule l NoTypeAnn [pat] (SimpleRhs expr) []])]
  ++ (concatMap transFormLocal lcs)
transFormLocal _ = []

-------------------------------------------------------------------------------
-- STATE FOR VARIABLEINDEX ----------------------------------------------------
-------------------------------------------------------------------------------

-- | State for variableindex
--   idx is the next free index
--   vmap contains all already seen variables and their index
data AHState = AHState { idx  :: Int
                       , vmap :: Map String Int
                       }

-- | Initionalstate for AHState
initialState = AHState 0 empty

-- | Returns the index of a variable
--   for an unseen variable the next possible state is returned
--   for a seen variable the given index is returned
getidx :: MonadState AHState m => String -> m Int
getidx name = do
    ahs <- get
    case DML.lookup name (vmap ahs) of
      Just x -> return x
      Nothing -> do
                   let idx' = idx ahs
                   put AHState {idx= idx' +1 , vmap=insert name idx' $ vmap ahs}
                   return idx'

-------------------------------------------------------------------------------
-- TRANSFORMATION HSE TO AH  --------------------------------------------------
-------------------------------------------------------------------------------

-- | Transforms a haskell-src-extensions module into an abstract haskell
--   program
hseToNLAH :: Module a -> Prog a
hseToNLAH modu = evalState (astToAbstractHaskell modu) initialState

astToAbstractHaskell :: MonadState AHState m => Module a -> m (Prog a)
astToAbstractHaskell modu@(Module l modh mp imps declas) =
  do
    let mn = parseModuleHead modh
    let ts = parseTypeSignatur modu
    let il = parseImportList imps
    tdcl <- mapM (parseTypDecls mn) $ filterdecls declas
    fdcl <- mapM (parseFunDecls mn ts) $ filterFunDecls declas
    return $ Prog (mn,l) il tdcl fdcl
astToAbstractHaskell _ = return $ Prog ("",undefined) [] [] []

-------------------------------------------------------------------------------
-- CREATION OF TYPEDECLS ------------------------------------------------------
-------------------------------------------------------------------------------

-- | Transforms a Typedeclaration
parseTypDecls :: MonadState AHState m => MName -> Decl l -> m (TypeDecl l)
parseTypDecls str (HSE.TypeDecl l declhead typ)               =
   do
     t <- parseTyp str typ
     tv <- parseTypeVariables typ
     return $ TypeSyn l (parseTypeName str declhead,l) Public tv t
parseTypDecls str (DataDecl l (DataType _) Nothing declhead qualcondecls _) =
  do
    etv <- mapM  extractTypvariables $ filterqual qualcondecls
    let he = concat etv
    qcd <- mapM (parseQualConDecl str) qualcondecls
    return $ Type l (parseTypeName str declhead,l) Public he qcd

-- | Extracts the typevariables out of a qualified constructor declaration
extractTypvariables ::
  MonadState AHState m => QualConDecl l -> m [(VarName, l)]
extractTypvariables (QualConDecl _ (Just tvb) _ _) =
  mapM parseTVB tvb

-- | Parses a typevariable
parseTVB :: MonadState AHState m => TyVarBind l -> m (VarName, l)
parseTVB (KindedVar l name _) = do
                                  y <- getidx (parsename name)
                                  return ((y, parsename name),l)
parseTVB (UnkindedVar l name) = do
                                  y <- getidx (parsename name)
                                  return ((y, parsename name),l)

-- | Parses a qualified constructer name
parseQualConDecl ::
  MonadState AHState m => MName -> QualConDecl l -> m (ConsDecl l)
parseQualConDecl str (QualConDecl _ _ _ conDecl) =
  parseConDecl str conDecl

-- | Parses a constructer declaration
parseConDecl :: MonadState AHState m => MName -> ConDecl l -> m (ConsDecl l)
parseConDecl str (ConDecl l name typs)       =
  do
    tps <- mapM (parseTyp str) typs
    return $ AH.Cons l ((str,parsename name),l) (length typs) Public tps
parseConDecl str (InfixConDecl l t1 name t2) =
  do
    tp1 <- parseTyp str t1
    tp2 <- parseTyp str t2
    let tp = tp1 : [tp2]
    return $ AH.Cons l ((str,parsename name),l) (length tp) Public tp
parseConDecl _  _                            =
  error "parseConDecl"

-- | Parses a typevariables out of a type
parseTypeVariables :: MonadState AHState m => Type l -> m [(VarName, l)]
parseTypeVariables (TyVar l name)     = do
                                          y <- getidx (parsename name)
                                          return [((y, parsename name),l)]
parseTypeVariables (TyFun _ t1 t2)    = do
                                          pv1 <- parseTypeVariables t1
                                          pv2 <- parseTypeVariables t2
                                          return $ pv1 ++ pv2
parseTypeVariables (TyTuple _ _ x)     = do
                                           pl <- mapM parseTypeVariables x
                                           let pv = concat pl
                                           return pv
parseTypeVariables (TyList _ t)        = parseTypeVariables t
parseTypeVariables (TyApp _ t1 t2)     = do
                                           pv1 <- parseTypeVariables t1
                                           pv2 <- parseTypeVariables t2
                                           return $ pv1 ++ pv2
parseTypeVariables (TyParen _ t)       = parseTypeVariables t
parseTypeVariables (TyInfix _ t1 _ t2) = do
                                           pv1 <- parseTypeVariables t1
                                           pv2 <- parseTypeVariables t2
                                           return $ pv1 ++ pv2
parseTypeVariables _                   = return []

-- | Parses a datatype name
parseTypeName :: String -> DeclHead l -> AH.QName
parseTypeName mn (DHead l name)     = (mn,parsename name)
parseTypeName mn (DHInfix l _ name) = (mn,parsename name)
parseTypeName mn (DHParen l dh)     = parseTypeName mn dh
parseTypeName mn (DHApp l dh _)     = parseTypeName mn dh

-------------------------------------------------------------------------------
-- CREATION OF FUNCDECLS ------------------------------------------------------
-------------------------------------------------------------------------------

type TypeS l = [(Name l,  Type l)]

-- | Parses a function declaration
parseFunDecls ::
  MonadState AHState m => MName -> TypeS l -> Decl l -> m (FuncDecl l)
parseFunDecls modu ts (FunBind l mas@(m:matches)) =
   do
     let fn = parseMatchName m
     put $ AHState {idx = 0 , vmap = empty}
     case (searchForType fn ts) of
       Nothing -> do
                    put $ AHState {idx = 0, vmap = empty}
                    rl <- mapM (parseRules modu ts) mas
                    let r = Rules rl
                    let fname = ((modu,fn),l)
                    let ar = parseArity mas
                    return $ Func l fname ar Public Untyped r
       Just z -> do
                   btd <- buildType l modu z
                   put $ AHState {idx = 0, vmap = empty}
                   rl <- mapM (parseRules modu ts) mas
                   let r = Rules rl
                   let fname = ((modu,fn),l)
                   let ar = parseArity mas
                   return $ Func l fname ar Public btd r
parseFunDecls modu ts (PatBind l pat rhs mbinds)  =
  do
    let pn = parseNameOutOfPattern pat
    rl <- parseRulesOutOfPats modu ts pat rhs
    case (searchForType pn ts) of
      Nothing -> return $ Func l ((modu,pn),l) 0 Public Untyped rl
      Just z -> do
                  btd <- buildType l modu z
                  return $ Func l ((modu,pn),l) 0 Public btd rl
parseFunDecls modu ts  _                          =
  do
    let fname = ((modu,""), undefined)
    return $ Func undefined fname 0 Public Untyped (Rules [])

-- | parses rules out of patterns
parseRulesOutOfPats ::
  MonadState AHState m => MName -> TypeS a -> Pat a -> HSE.Rhs a -> m (Rules a)
parseRulesOutOfPats  modu ts pat rhs  =
  do
    rs <- parseRule modu ts [pat] rhs
    return $ Rules [rs]

-- | Parses rules
parseRules ::
  MonadState AHState m => MName -> TypeS a -> Match a -> m (AH.Rule a)
parseRules str t (Match l _ pats rhs wbinds) =
  do
    r@(AH.Rule l tya p rh loc) <- parseRule str t pats rhs
    case wbinds of
      Nothing -> return $ AH.Rule l tya p rh (loc)
      Just y -> do
                  locs <- (parseBinds str t) y
                  return $ AH.Rule l tya p rh (loc ++ locs)

-- | Parses a rule
parseRule ::
  MonadState AHState m => MName   ->
                          TypeS a ->
                          [Pat a] ->
                          HSE.Rhs a ->
                           m (AH.Rule a)
parseRule str t pats (UnGuardedRhs l expr)  =
  do
    patt <- mapM (parsePatterns str) pats
    ep <- parseExpr str t expr
    return $ AH.Rule l NoTypeAnn patt (SimpleRhs ep) []
parseRule str t pats (GuardedRhss l gurhss) =
  do
    patt <- mapM (parsePatterns str) pats
    g <- mapM (parseGuarded str t) gurhss
    return $ AH.Rule l NoTypeAnn patt (AH.GuardedRhs l (concat g)) []

-- | Parses a guarded right hand side
parseGuarded ::
  MonadState AHState m => MName ->
                          TypeS l ->
                          GuardedRhs l ->
                          m [(Expr l, Expr l)]
parseGuarded str t (HSE.GuardedRhs l stmts expr) =
  mapM (splitStatments str t expr) stmts

-- | Splits a statements into (expr1,expr2) with expr1 is the parsed statement
splitStatments ::
  MonadState AHState m => MName ->
                          TypeS l ->
                          Exp l ->
                          Stmt l ->
                          m (Expr l, Expr l)
splitStatments str t expr stmts =
  do
    expr2 <- parseExpr str t expr
    expr1 <- parseStmtsToExpr str t $ head  $ filterStmts [stmts]
    return (expr1,expr2)

-- | Parses a statement into an expr
parseStmtsToExpr ::
  MonadState AHState m => MName -> TypeS a -> Stmt a -> m (Expr a)
parseStmtsToExpr str t (Qualifier l expr) =
  parseExpr str t expr


-- | Parses functions and pattern bindings
parseFuncPatDecls ::
  MonadState AHState m => String -> TypeS a -> [Decl a] -> m [LocalDecl a]
parseFuncPatDecls _ _ []                                      =
  return []
parseFuncPatDecls str t ((FunBind l matches@(m:ms)):xs)       =
  do
    let fn = parseMatchName  m
    case (searchForType fn t) of
      Nothing -> do
                   rl <- mapM (parseRules str t) matches
                   let r = Rules rl
                   let fname = ((str,fn),l)
                   let ar = parseArity matches
                   return $ [LocalFunc $ Func l fname ar Public Untyped r]
      Just z -> do
                  btd <- buildType l str z
                  rl <- mapM (parseRules str t) matches
                  let r = Rules rl
                  let fname = ((str,fn),l)
                  let ar = parseArity matches
                  return $ [LocalFunc $ Func l fname ar Public btd r]
parseFuncPatDecls str t
  (PatBind l pat rhs@(UnGuardedRhs a expr) (Just wbind) : xs) =
   do
    rh <- parseExprOutOfRhs str t rhs
    patt <- parsePatterns str pat
    bnd <- parseBinds str t wbind
    return [LocalPat l patt rh bnd]
parseFuncPatDecls str t
  ((PatBind l pat rhs@(GuardedRhss a gds) (Just b)):xs) |length gds <= 1 =
   do
     rh <- parseExprOutOfGrd str t (head gds)
     patt <- parsePatterns str pat
     bnd <- parseBinds str t b
     return $ [LocalPat l patt rh bnd]
                                                        |otherwise      =
   do
     let name = parseNameOutOfPattern pat
     rh <- parseExprOutOfGrd str t (head gds)
     patt <- parsePatterns str pat
     bnd <- parseBinds str t b
     rules <- mapM (parseRulesOutOfGuarded str t) gds
     let rulesR = makeRules rules
     let n = newRule l patt rulesR bnd
     return $ [LocalFunc $ Func l (("",name),l) 0 Public Untyped (Rules [n])]
parseFuncPatDecls _  _  _                                     =
  return []

-- | Builds a new Rule
newRule :: a -> Pattern a -> AH.Rhs a -> [LocalDecl a] -> AH.Rule a
newRule l pat rhs b = AH.Rule l NoTypeAnn [pat] rhs b

-- | Makes one right hand side out of a list of right hand sides
makeRules :: [AH.Rhs a] -> AH.Rhs a
makeRules xs = let r =  makeRules' xs
                  in SimpleRhs $ AH.List undefined NoTypeAnn r

-- | transforms tupel into a list
toExprList :: [(t, t)] -> [t]
toExprList  [] =[]
toExprList ((a,b):xs) = a: b: toExprList xs

makeRules' :: [AH.Rhs t] -> [Expr t]
makeRules' ((AH.GuardedRhs a es):xs) = (toExprList es) ++ makeRules' xs

parseRulesOutOfGuarded
  :: MonadState AHState m => MName -> TypeS a -> GuardedRhs a -> m (AH.Rhs a)
parseRulesOutOfGuarded str t (HSE.GuardedRhs l stmts expr) = do
  tups <- mapM (parseTupels str t expr) stmts
  return $ AH.GuardedRhs l tups


-- | Parses a tupel
parseTupels ::
 MonadState AHState m => MName ->
                         TypeS a ->
                         Exp a ->
                         Stmt a ->
                         m (Expr a, Expr a)
parseTupels str t expr2 (Qualifier l expr1) =
  do
    ex1 <- parseExpr str t expr1
    ex2 <- parseExpr str t expr2
    return (ex1,ex2)

-- | Parses an expr out of a unguarded right hand side
parseExprOutOfRhs ::
  MonadState AHState m => MName -> TypeS a -> HSE.Rhs a -> m (Expr a)
parseExprOutOfRhs str t (UnGuardedRhs l expr) =
  parseExpr str t expr

-- | Parses an expr out of a guarded right hand side
parseExprOutOfGrd ::
  MonadState AHState m => MName -> TypeS a -> GuardedRhs a -> m (Expr a)
parseExprOutOfGrd str t (HSE.GuardedRhs l stmts expr) =
  parseExpr str t expr

-- | parses a right hand side
parseRigthHands ::
  MonadState AHState m => MName -> TypeS a -> HSE.Rhs a -> m (AH.Rhs a)
parseRigthHands str t (UnGuardedRhs l e)   =
  do
    expr <- parseExpr str t e
    return $ SimpleRhs expr
parseRigthHands str t (GuardedRhss l grhs) =
  do
    gud <- mapM (parseGuarded str t) grhs
    return $ AH.GuardedRhs l (concat gud)

-- | parses a Local declaration out of an expr
parseLocal ::
  MonadState AHState m => String -> TypeS a -> Exp a -> m [LocalDecl a]
parseLocal str t (HSE.Let l binds e)  = parseBinds str t binds
parseLocal _ _ _                      = return []

-- | parses an expr
parseExpr :: MonadState AHState m => MName -> TypeS a -> Exp a -> m (Expr a)
parseExpr _  _ (HSE.Var l qn)                    =
  do
    y <- getidx (parseQName qn)
    return $ AH.Var NoTypeAnn ((y,parseQName qn),l)
parseExpr mn _ (Con l qn)                        =
  return $ AH.Symbol NoTypeAnn  ((mn,parseQName qn), l)
parseExpr _  _ (HSE.Lit l lit)                   =
  return $ AH.Lit NoTypeAnn (parseLiteral lit, l)
parseExpr mn t (InfixApp l exp1 qop exp2)        =
  do
    expr1 <- parseExpr mn t exp1
    expr2 <- parseExpr mn t exp2
    return $ InfixApply l NoTypeAnn expr1 (parseQOp mn qop, l) expr2
parseExpr mn t (HSE.Lambda l pats e)             =
  do
    expr <- parseExpr mn t e
    pat <- mapM (parsePatterns mn) pats
    return $ AH.Lambda l NoTypeAnn pat expr
parseExpr mn t (HSE.Let l binds e)               =
  do
    expr <- parseExpr mn t e
    bnd <- parseBinds mn t binds
    return $ AH.Let l NoTypeAnn bnd expr
parseExpr mn t (If l e1 e2 e3)                   =
  do
    expr1 <- parseExpr mn t e1
    expr2 <- parseExpr mn t e2
    expr3 <- parseExpr mn t e3
    return $ IfThenElse l NoTypeAnn expr1 expr2 expr3
parseExpr mn t (HSE.Case l e alters)             =
  do
    expr <- parseExpr mn t e
    alt <- mapM (parseAlternatives mn t) alters
    return $ AH.Case l NoTypeAnn expr alt
parseExpr mn t (Do  l stms)                      =
  do
    st <- mapM (parseStms mn t) stms
    return $ DoExpr l NoTypeAnn st
parseExpr mn t (HSE.Tuple l Boxed es)            =
  do
    e <- mapM (parseExpr mn t) es
    return $ AH.Tuple l NoTypeAnn e
parseExpr mn t (HSE.List l exprs)                =
  do
    eps <- mapM (parseExpr mn t) exprs
    return $ AH.List l NoTypeAnn eps
parseExpr mn t (Paren _ e)                       =
  parseExpr mn t e
parseExpr mn t (App l e1 e2)                     =
  do
    expr1 <- parseExpr mn t e1
    expr2 <- parseExpr mn t e2
    return $ Apply l NoTypeAnn expr1 expr2
parseExpr mn t (HSE.ListComp l e qs)             =
  do
    expr <- parseExpr mn t e
    let r = filterQualsStmts qs
    q <- mapM (parseQualsStms mn t) r
    return $ AH.ListComp l NoTypeAnn expr q
parseExpr mn t (ExpTypeSig l expr typ)           =
  do
    e <- parseExpr mn t expr
    t <- parseTyp mn typ
    return $ Typed l NoTypeAnn e t
parseExpr mn t (EnumFrom l expr)                 =
  do
    e <- parseExpr mn t expr
    return $ Apply l NoTypeAnn (AH.Lit NoTypeAnn (Stringc "enumFrom", l)) e
parseExpr mn t (EnumFromTo l exp1 exp2)          =
  do
    expr1 <- parseExpr mn t exp1
    expr2 <- parseExpr mn t exp2
    return $ Apply l NoTypeAnn (AH.Lit NoTypeAnn (Stringc "enumFromTo",l))
                               (Apply l NoTypeAnn expr1 expr2)
parseExpr mn t (EnumFromThen l exp1 exp2)        =
  do
    expr1 <- parseExpr mn t exp1
    expr2 <- parseExpr mn t exp2
    return $ Apply l NoTypeAnn (AH.Lit NoTypeAnn (Stringc "enumFromThen",l))
                              (Apply l NoTypeAnn expr1 expr2)
parseExpr mn t (EnumFromThenTo l exp1 exp2 exp3) =
  do
    expr1 <- parseExpr mn t exp1
    expr2 <- parseExpr mn t exp2
    expr3 <- parseExpr mn t exp3
    return $ Apply l NoTypeAnn (AH.Lit NoTypeAnn (Stringc "enumFromThenTo",l))
                   (Apply l NoTypeAnn  expr1 (Apply l NoTypeAnn expr2 expr3))
parseExpr _  _ _                                 =
  error "parseExpr"

-- | Transforms a right hand side to an expr
rightHandtoExp ::
  MonadState AHState m => MName -> TypeS a -> HSE.Rhs a -> m (Expr a)
rightHandtoExp str t (UnGuardedRhs _ e)    = parseExpr str t e
rightHandtoExp str t (GuardedRhss _ gdrhs) = error "rightHandtoExp"

-- | Parses a pattern
parsePatterns:: MonadState AHState m => MName -> Pat l -> m (Pattern l)
parsePatterns mn (HSE.PVar l name)         =
  do
    y <- getidx (parsename name)
    return $ AH.PVar NoTypeAnn ((y,parsename name),l)
parsePatterns mn (HSE.PLit l sign lit)     =
  return $ AH.PLit NoTypeAnn (parseLiteral lit, l)
parsePatterns mn (PApp l qn pats)          =
  do
    pat <- mapM (parsePatterns mn) pats
    return $ PComb l NoTypeAnn ((mn,parseQName qn),l) pat
parsePatterns mn (HSE.PTuple l Boxed pats) =
  do
    pat <- mapM (parsePatterns mn) pats
    return $ AH.PTuple l NoTypeAnn pat
parsePatterns mn (HSE.PList l pats)        =
  do
    pat <- mapM (parsePatterns mn) pats
    return $ AH.PList l NoTypeAnn pat
parsePatterns mn (PParen _ pat)            =
  parsePatterns mn pat
parsePatterns mn (PAsPat l name pat)       =
  do
    y <- getidx (parsename name)
    patt <- parsePatterns mn pat
    return $ PAs l NoTypeAnn ((y,parsename name),l) patt
parsePatterns _  _                         =
  error "parsePatterns"

-- | parses a statement
parseStms ::
  MonadState AHState m => MName -> TypeS a -> Stmt a -> m (Statement a)
parseStms str t (Generator l pat e) =
  do
    patt <- parsePatterns str pat
    expr <- parseExpr str t e
    return $ SPat l  patt expr
parseStms str t (Qualifier l e)     =
  do
    expr <- parseExpr str t e
    return $ SExpr expr
parseStms str t (LetStmt l binds)   =
  do
    bnd <- parseBinds str t binds
    return $ SLet l bnd
parseStms _  _ _                    =
  error "parseStms"

-- | Parses a binding
parseBinds ::
  MonadState AHState m => String -> TypeS a -> Binds a -> m [LocalDecl a]
parseBinds str t (BDecls _ decls) = parseFuncPatDecls str t decls
parseBinds _ _ _                  = return []

-- | Parses an alternative
parseAlternatives ::
  MonadState AHState m => MName -> TypeS a -> Alt a -> m (BranchExpr a)
parseAlternatives str t (Alt l pat rhs _) =
  do
    patt <- parsePatterns str pat
    rh <- rightHandtoExp str t rhs
    return $ Branch l patt rh

-- | Parses an QOp
parseQOp :: String -> QOp l -> AH.QName
parseQOp mn (QVarOp l qn) = (mn,parseQName qn)
parseQOp mn (QConOp l qn) = (mn, parseQName qn)

-- | Parses an qualified statement
parseQualsStms ::
  MonadState AHState m => MName -> TypeS a -> QualStmt a -> m (Statement a)
parseQualsStms str t (QualStmt _ stm ) = parseStms str t stm

-- | Parses a literal
parseLiteral :: HSE.Literal l -> AH.Literal
parseLiteral (Char _ c _)     = Charc c
parseLiteral (String _ str _) = Stringc str
parseLiteral (Int _ i _)      = Intc $ fromInteger i
parseLiteral (Frac _ r _)     = Floatc $ fromRational r
parseLiteral _                = error "parseLiteral"

-- | Parses a type
parseTyp :: MonadState AHState m => MName -> Type a -> m (TypeExpr a)
parseTyp _    (TyVar l name)          =
  do
    y <- getidx (parsename name)
    return $ TVar ((y, parsename name),l)
parseTyp modu (TyFun l t1 t2)         =
  do
    ty1 <- parseTyp modu t1
    ty2 <- parseTyp modu t2
    return $ FuncType l ty1 ty2
parseTyp modu (TyTuple l Boxed types) =
  do
    ty <- mapM (parseTyp modu) types
    return $ TCons l ((modu, "tupel"),l) ty
parseTyp modu (TyList l typ)          =
  do
    ty <- parseTyp modu typ
    return $ TCons l ((modu, "list"),l) [(ty)]
parseTyp modu (TyCon l qname)         =
  return $ TCons l ((modu, parseQName qname),l) []
parseTyp modu (TyParen l t)           =
  do
    ty <- parseTyp modu t
    return $ TCons l ((modu, "paren"),l) [(ty)]
parseTyp modu  _                      =
  error "parseTyp"

-- | Parses the arity
parseArity :: [Match l] -> Int
parseArity []                                      = 0
parseArity ((InfixMatch {}):ms)                    = 2
parseArity (Match l name patterns rhs wbinds : ms) = length patterns

-- | Parses the importlist
parseImportList :: [ImportDecl l] -> [(MName, l)]
parseImportList [] = []
parseImportList (ImportDecl l name _ _ _ _ _ _ :xs) =
  (parseModuleName name,l) : parseImportList xs

-- | Parses the modulehead
parseModuleHead :: Maybe (ModuleHead l) -> MName
parseModuleHead Nothing                     = ""
parseModuleHead (Just (ModuleHead l n _ _)) = parseModuleName n

-- | parses the module name
parseModuleName :: ModuleName l -> String
parseModuleName (ModuleName l str) = str

-- | Parses decls
parseDecls :: [Decl l] -> TypeS l
parseDecls = Prelude.foldr ((++) . parseOneDecl) []

-- | Parses a single decl
parseOneDecl :: Decl l -> TypeS l
parseOneDecl (HSE.TypeSig l names t) = concatMap (parseTypeSig t) names
parseOneDecl _ = []

-------------------------------------------------------------------------------
-- HELPING FUNCTIONS ----------------------------------------------------------
-------------------------------------------------------------------------------

-- | Parses a match name
parseMatchName :: Match l -> String
parseMatchName (Match l name patterns rhs wbinds)       = parsename name
parseMatchName (InfixMatch l pat1 name pat2 rhs wbinds) = parsename name

-- | Parses a name
parsename :: Name l -> String
parsename (Ident l name)                        = name
parsename (HSE.Symbol l name) = name

-- | Parses a qname
parseQName :: HSE.QName l -> String
parseQName (Qual l mdn name) = parsename name
parseQName (UnQual l name)   = parsename name
parseQName _                 = ""

-- | Look of the type of a function is defined in the module
searchForType :: String -> TypeS l -> Maybe (Type l)
searchForType _ [] = Nothing
searchForType name ((n , t):nts) | name == (parsename n) =  Just t
                                 | otherwise = searchForType name nts

-- | Generates a typesignature
buildType :: MonadState AHState m => a -> MName -> Type a -> m (TypeSig a)
buildType l modname x          = do
                                   t <- parseTyp modname x
                                   return $ AH.TypeSig t

-- | Returns the name to a ah pattern
parseNamePattern :: Pattern l -> AH.QName
parseNamePattern (AH.PVar t ((i,s),l))   = ("",s)
parseNamePattern (PComb l t (qn,a) pats) = qn
parseNamePattern (PAs a t ((i,s),l) pat) = ("",s)
parseNamePattern (AH.PLit t (l, a))      = ("", "Literal")
parseNamePattern (AH.PTuple a t pats)    = ("","Tupel")
parseNamePattern (AH.PList a t pats)     = ("","List")

-- | Returns the name to a hse pattern
parseNameOutOfPattern :: Pat l -> String
parseNameOutOfPattern (HSE.PVar l name)         =
  parsename name
parseNameOutOfPattern (HSE.PTuple l Boxed pats) =
  "Tupel " ++ concatMap parseNameOutOfPattern pats
parseNameOutOfPattern (HSE.PList l pats)        =
  "Liste " ++ concatMap parseNameOutOfPattern pats
parseNameOutOfPattern (PParen _ pat)            =
  parseNameOutOfPattern pat
parseNameOutOfPattern (PAsPat l name pat)       =
  parsename name


-- | Adds a variable to a list of patterns
addToPatterns :: [VarName] -> [Pattern l] -> l -> [Pattern l]
addToPatterns xs ys l =
 Prelude.foldl (\ys x -> ys ++ [AH.PVar NoTypeAnn (x, l)])
               ys
               xs

-- | Returns the difference of two lists
findDifference :: Eq a => [a] -> [a] -> [a]
findDifference xs ys = [y | y <- ys, y `notElem` xs]

-- | For functions declarations only
--   FunBind,
--   Patbind,
--   are allowed
filterFunDecls :: [Decl l] -> [Decl l]
filterFunDecls []                                 = []
filterFunDecls (x@(FunBind l mas@(m:matches)):xs) = x:filterFunDecls xs
filterFunDecls (x@PatBind{} : xs)                 = x:filterFunDecls xs
filterFunDecls (x:xs)                             = filterFunDecls xs

-- | For typedeclarations only
--   TypeDecl,
--   DataDecls as DataType,
--   are allowed
filterdecls :: [Decl l] -> [Decl l]
filterdecls []                                             = []
filterdecls (x@HSE.TypeDecl{} : xs)      = x : filterdecls xs
filterdecls (x@(DataDecl _ (DataType _) Nothing _ _ _):xs) = x : filterdecls xs
filterdecls (x:xs)                                         = filterdecls xs

-- | For qualified constructer declarations only
--   QualConDecl
--   is allowed
filterqual :: [QualConDecl l] -> [QualConDecl l]
filterqual []                                    = []
filterqual (x@(QualConDecl _ (Just tvb) _ _):xs) = x : filterqual xs
filterqual (x:xs)                                = filterqual xs

-- | For Statements here is only
--   Qualifier
--   allowed
filterStmts :: [Stmt a] -> [Stmt a]
filterStmts []                        = []
filterStmts (x@(Qualifier l expr):xs) = x : filterStmts xs
filterStmts (x:xs)                    = filterStmts xs

-- | For qualified statements only
--   QualStmt
--   is allowed
filterQualsStmts :: [QualStmt l] -> [QualStmt l]
filterQualsStmts []                       = []
filterQualsStmts (x@(QualStmt _ stm ):xs) = x : filterQualsStmts xs
filterQualsStmts (x:xs)                   = filterQualsStmts xs
