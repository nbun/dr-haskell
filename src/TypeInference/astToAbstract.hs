{-# LANGUAGE FlexibleContexts #-}

import Language.Haskell.Exts
import Data.Functor
import TypeInference.AbstractHaskell
import Control.Monad.State.Lazy
import Data.Map.Lazy


------------------------------------------------------------------------------------------------------------------
-- TESTPROGRAMM --------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------

--f x y = h 5
-- where
--   h z = x


testp = Prog (("TestName"), "string") [] [] [f]

f = Func "string" (("TestName","f"), "string") 2 Public Untyped (Rules r)

r = [TypeInference.AbstractHaskell.Rule "string" NoTypeAnn [TypeInference.AbstractHaskell.PVar NoTypeAnn ((0,"x"), "string"),TypeInference.AbstractHaskell.PVar NoTypeAnn ((1,"y"),"string")] (SimpleRhs $ Apply "string" NoTypeAnn (TypeInference.AbstractHaskell.Var TypeInference.AbstractHaskell.NoTypeAnn ((2,"h"),"string")) (TypeInference.AbstractHaskell.Lit NoTypeAnn (Intc 5,"string"))) [LocalFunc t]]

t = Func "string"(("TestName","h"), "string") 1 Private Untyped (Rules s)

s = [TypeInference.AbstractHaskell.Rule "string" NoTypeAnn [TypeInference.AbstractHaskell.PVar NoTypeAnn ((3,"z"), "string")] (SimpleRhs (TypeInference.AbstractHaskell.Var NoTypeAnn ((0,"x") ,"string"))) []]

dst = (SimpleRhs $ Apply "string" NoTypeAnn (TypeInference.AbstractHaskell.Var TypeInference.AbstractHaskell.NoTypeAnn ((2,"h"),"string")) (TypeInference.AbstractHaskell.Lit NoTypeAnn (Intc 5,"string")))
-------------------------------------------------------------------------------------------------------------------------------------------------------
-- LAMBDA LIFTING FÜR LOKALE DEKLARATIONEN ------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------
lambdaLifting :: Prog l -> Prog l
lambdaLifting prog =  evalState (lambdaProg prog) initialStateLambda

initialStateLambda = LambdaState empty empty

lambdaProg :: MonadState LambdaState m => Prog l -> m (Prog l )
lambdaProg p@(Prog m n t fs) = do
   p1 <- addFreeVariablesInProg p -- Parameter ergänzen
   v@(Prog n i t fd) <- abstrHaskToAbstrRepr p1 -- Namen erweitern -- auch noch in zweiten argument
   let list = transFormLocalProg [] v -- liften
   let newProg = Prog n i t (fd ++ list)
   return newProg

abstrHaskToAbstrRepr :: MonadState LambdaState m => Prog a -> m (Prog a)
abstrHaskToAbstrRepr (Prog name imps types fundecls) =
  do
  funDefN <- mapM abstrReprFuncDef fundecls
  return $ Prog name imps types funDefN
  
abstrReprFuncDef :: MonadState LambdaState m => FuncDecl a -> m (FuncDecl a)
abstrReprFuncDef (Func a name arity visibility tsig rules)  =
  do
    rulesN <- abstrReprRules (snd $ fst name) rules
    return $ Func a name arity visibility tsig rulesN

abstrReprRules :: MonadState LambdaState m => String -> Rules a -> m (Rules a)
abstrReprRules name (Rules rules) =
  do
    newRules <- mapM (abstrReprRule name) rules
    return $ Rules newRules

abstrReprRule :: MonadState LambdaState m => String -> TypeInference.AbstractHaskell.Rule a -> m (TypeInference.AbstractHaskell.Rule a)
abstrReprRule name (TypeInference.AbstractHaskell.Rule a tanno pats rhs localdecls) = do
   rhside <- abstrReprRhs name rhs
   ldcls <- mapM (abstrReprLocal name) localdecls
   return $ TypeInference.AbstractHaskell.Rule a tanno pats rhside ldcls

abstrReprRhs :: MonadState LambdaState m => String -> TypeInference.AbstractHaskell.Rhs a -> m (TypeInference.AbstractHaskell.Rhs a)
abstrReprRhs name (SimpleRhs expr) = do
  exprN <- abstrReprExpr name expr
  return  $ SimpleRhs exprN
abstrReprRhs name (TypeInference.AbstractHaskell.GuardedRhs a exprsTups) = do
  newTups <- mapM (abstrReprExprTups name) exprsTups
  return $ TypeInference.AbstractHaskell.GuardedRhs a newTups

abstrReprExprTups :: MonadState LambdaState m => String -> (Expr a, Expr a1) -> m (Expr a, Expr a1)
abstrReprExprTups name (expr1,expr2) =
  do
    exprN1 <- abstrReprExpr name expr1
    exprN2 <- abstrReprExpr name expr2
    return (exprN1,exprN2)

abstrReprLocal :: MonadState LambdaState m => String -> LocalDecl a -> m (LocalDecl a)
abstrReprLocal name (LocalFunc funcdecls) = do
  x <- abstrReprFuncDef funcdecls
  return $ LocalFunc x
abstrReprLocal name (LocalPat a pat expr locals) = do
  exprN <- abstrReprExpr name expr
  localsN <- mapM (abstrReprLocal name) locals
  return $ LocalPat a pat exprN localsN

abstrReprExpr :: MonadState LambdaState m => String -> Expr a -> m (Expr a)
abstrReprExpr name x@(TypeInference.AbstractHaskell.Var tanno vname)              =
  do
    lds <- get
    let realName = snd $ fst vname
    let newName =  name ++  "." ++ realName
    case Data.Map.Lazy.lookup realName (frees lds) of
     Nothing -> return x
     Just y -> return $ TypeInference.AbstractHaskell.Var tanno ((fst $ fst vname ,newName),(snd vname))
abstrReprExpr name x@(TypeInference.AbstractHaskell.Lit tanno lit)                = return x
abstrReprExpr name x@(TypeInference.AbstractHaskell.Symbol tyanno qname)          = return x
abstrReprExpr name x@(TypeInference.AbstractHaskell.Lambda a tyanno pats expr)    = do
  exprN <- abstrReprExpr name expr
  return $ TypeInference.AbstractHaskell.Lambda a tyanno pats exprN
abstrReprExpr name x@(TypeInference.AbstractHaskell.Let a tyanno locals expr)     = do
   newLocals <- mapM (abstrReprLocal name) locals
   newExpr   <- abstrReprExpr name expr
   return $ TypeInference.AbstractHaskell.Let a tyanno newLocals newExpr
abstrReprExpr name x@(DoExpr a tyanno stms)                                         = do
  stmtN <- mapM (abstrReprStmts name) stms
  return $  DoExpr a tyanno stmtN
abstrReprExpr name x@(TypeInference.AbstractHaskell.ListComp a tyanno expr stmts) = do
  exprN <- abstrReprExpr name expr
  stmtN <- mapM (abstrReprStmts name) stmts
  return $TypeInference.AbstractHaskell.ListComp a tyanno exprN stmtN
abstrReprExpr name x@(TypeInference.AbstractHaskell.Case a tyanno expr bes)       = do
  exprN <- abstrReprExpr name expr
  bexp <- mapM (abstrReprBranchExpr name) bes
  return $ TypeInference.AbstractHaskell.Case a tyanno exprN bexp
abstrReprExpr name x@(Typed a tyanno expr texpr)                                  = do
  exprN <- abstrReprExpr name expr
  return $ Typed a tyanno exprN texpr
abstrReprExpr name x@(IfThenElse a tyanno expr1 expr2 expr3)                      = do
  exprN1 <- abstrReprExpr name expr1
  exprN2 <- abstrReprExpr name expr2
  exprN3 <- abstrReprExpr name expr3
  return $  IfThenElse a tyanno exprN1 exprN2 exprN3
abstrReprExpr name x@(TypeInference.AbstractHaskell.Tuple a tyanno exprs)         = do
  exprsN <- mapM (abstrReprExpr name) exprs
  return $ TypeInference.AbstractHaskell.Tuple a tyanno exprsN
abstrReprExpr name x@(TypeInference.AbstractHaskell.List a tyanno exprs)          = do
  exprsN <- mapM (abstrReprExpr name) exprs
  return $ TypeInference.AbstractHaskell.List a tyanno exprsN
abstrReprExpr name x@(Apply a tyanno z@(TypeInference.AbstractHaskell.Var tanno vname) expr2) = do
  lds <- get
  case Data.Map.Lazy.lookup (snd $ fst vname) (frees lds) of
    Nothing -> do
               z' <- abstrReprExpr name z
               exprN2 <- abstrReprExpr name expr2
               return $ Apply a tyanno z' exprN2
    Just y -> do
                expr2Ext <- extendParameters a (snd $ fst vname)  expr2
                ex2 <- abstrReprExpr name expr2Ext
                z' <-  abstrReprExpr name z
                return $ Apply a tyanno z' ex2
abstrReprExpr name x@(Apply a tyanno expr1 expr2)                                 = do
  exprN1 <- abstrReprExpr name expr1
  exprN2 <- abstrReprExpr name expr2
  return $ Apply a tyanno exprN1 exprN2
abstrReprExpr name x@(InfixApply a tyanno z@(TypeInference.AbstractHaskell.Var tanno vname) qname expr2) = do
  lds <- get
  case Data.Map.Lazy.lookup (snd $ fst vname) (frees lds) of
    Nothing -> do
               z' <- abstrReprExpr name z
               exprN2 <- abstrReprExpr name expr2
               return $ InfixApply a tyanno z' qname exprN2
    Just y -> do
              expr2Ext <- extendParameters a (snd $ fst vname)  expr2
              ex2 <- abstrReprExpr name expr2Ext
              z' <-  abstrReprExpr name z
              return $ InfixApply a tyanno z' qname ex2
abstrReprExpr name x@ (InfixApply a tyanno expr1 qname expr2)                     = do
  exprN1 <- abstrReprExpr name expr1
  exprN2 <- abstrReprExpr name expr2
  return $ InfixApply a tyanno exprN1 qname exprN2

extendParameters :: MonadState LambdaState m => a -> String -> Expr a -> m (Expr a)
extendParameters b name x@(TypeInference.AbstractHaskell.List a tyanno exprs) = do
  lds <- get
  let  values = (frees lds) ! name
  let eprVars = transformVars a values exprs
  return $ TypeInference.AbstractHaskell.List a tyanno eprVars
extendParameters a name x                                                   = do
  lds <- get
  let values = (frees lds) ! name
  let exprVars = transformVars a values [x]
  return $ TypeInference.AbstractHaskell.List a NoTypeAnn exprVars

transformVars :: a -> [VarName] -> [Expr a] -> [Expr a]
transformVars a  [] z = z
transformVars a (x:xs) y = transformVars a xs (y ++[(TypeInference.AbstractHaskell.Var NoTypeAnn (x, a ))] )

filteringVNames :: [(Int,String)] -> [String]
filteringVNames []             = []
filteringVNames ((i,s):xs) = [s] ++ filteringVNames xs

abstrReprStmts :: MonadState LambdaState m => String -> Statement a -> m (Statement a)
abstrReprStmts name (SExpr expr)      =
  do
    expr1 <- abstrReprExpr name expr
    return $ SExpr expr1
abstrReprStmts name (SPat a pat expr) =
  do
    expr1 <- abstrReprExpr name expr
    return $ SPat a pat expr1
abstrReprStmts name (SLet a locals)   =
  do
    newLocals <- mapM (abstrReprLocal name) locals
    return $ SLet a newLocals

abstrReprBranchExpr :: MonadState LambdaState m => String -> BranchExpr a -> m (BranchExpr a)
abstrReprBranchExpr name (Branch a pat expr) =
  do
    exprN <- abstrReprExpr name expr
    return $ Branch a pat exprN

--------------------------------------------------------------------------------------------------------------------
data LambdaState  = LambdaState {locals :: Map String Int -- wie viele freie Variablen in einer Funktion
                               , frees :: Map String [VarName] -- welche Variablen wurden zu welcher Funktion ergänzt
                               }

getCountOfFrees :: MonadState LambdaState m => String -> m Int
getCountOfFrees name = do
  lds <- get
  case Data.Map.Lazy.lookup name (locals lds) of
    Nothing -> return 0
    Just x -> return x

addFreeVariablesInProg :: MonadState LambdaState m => Prog l -> m (Prog l)
addFreeVariablesInProg (Prog n x y fundecls) =
  do
    newFuncDecls <- mapM addFreeVariablesInFuncDecls fundecls
    return $ Prog n x y newFuncDecls

addFreeVariablesInFuncDecls :: MonadState LambdaState m => FuncDecl l -> m (FuncDecl l)
addFreeVariablesInFuncDecls (Func x y z a b rules) = do
  newRules <- addFreeVariablesInRules rules
  return $ Func x y z a b newRules

addFreeVariablesInRules :: MonadState LambdaState m => Rules l -> m (Rules l)
addFreeVariablesInRules (Rules rule) = do
  newRule <- mapM addFreeVariablesInRule rule
  return $ Rules newRule

addFreeVariablesInRule :: MonadState LambdaState m => TypeInference.AbstractHaskell.Rule l -> m (TypeInference.AbstractHaskell.Rule l)
addFreeVariablesInRule (TypeInference.AbstractHaskell.Rule a b c d e) =
  do
    newRhs <- addFreeVariablesInRhs d
    newLocals <- mapM addFreeVariablesInLocals e
    return $ TypeInference.AbstractHaskell.Rule  a b c newRhs newLocals

addFreeVariablesInRhs :: MonadState LambdaState  m => TypeInference.AbstractHaskell.Rhs l -> m(TypeInference.AbstractHaskell.Rhs l)
addFreeVariablesInRhs (SimpleRhs expr) =
  do
    newExpr <- addFreeVariablesInExpr expr
    return $ SimpleRhs newExpr
addFreeVariablesInRhs (TypeInference.AbstractHaskell.GuardedRhs a exprs) =
  do
    newExprList <- mapM addFreeVariablesInExprList exprs
    return $ TypeInference.AbstractHaskell.GuardedRhs a newExprList

addFreeVariablesInExprList :: MonadState LambdaState m => (Expr a, Expr a) -> m(Expr a, Expr a)
addFreeVariablesInExprList (a,b) =
  do
    newA <- addFreeVariablesInExpr a
    newB <- addFreeVariablesInExpr b
    return $ (newA , newB)

addFreeVariablesInExpr :: MonadState LambdaState m => Expr l -> m (Expr l)
addFreeVariablesInExpr x@(TypeInference.AbstractHaskell.Var _ _)                    = return x
addFreeVariablesInExpr x@(TypeInference.AbstractHaskell.Lit _ _)                    = return x
addFreeVariablesInExpr x@(TypeInference.AbstractHaskell.Symbol _ _)                 = return x
addFreeVariablesInExpr (Apply a tyanno expr1 expr2)                                 =
  do
    newExpr1 <- addFreeVariablesInExpr expr1
    newExpr2 <- addFreeVariablesInExpr expr2
    return $ Apply a tyanno newExpr1 newExpr2
addFreeVariablesInExpr (InfixApply a tyanno expr1 name expr2)                       =
  do
    newExpr1 <- addFreeVariablesInExpr expr1
    newExpr2 <- addFreeVariablesInExpr expr2
    return $ InfixApply a tyanno newExpr1 name newExpr2
addFreeVariablesInExpr (TypeInference.AbstractHaskell.Case a tyanno expr bexprs)    =
  do
    newExpr <- addFreeVariablesInExpr expr
    newBExprs <- mapM addFreeVariablesInBExprs bexprs
    return $ TypeInference.AbstractHaskell.Case a tyanno expr bexprs
addFreeVariablesInExpr (Typed a tyanno expr texpr)                                  =
  do
    newExpr <- addFreeVariablesInExpr expr
    return $ Typed a tyanno newExpr texpr
addFreeVariablesInExpr (IfThenElse a tyanno expr1 expr2 expr3)                      =
  do
    newExpr1 <- addFreeVariablesInExpr expr1
    newExpr2 <- addFreeVariablesInExpr expr2
    newExpr3 <- addFreeVariablesInExpr expr3
    return $ IfThenElse a tyanno newExpr1 newExpr2 newExpr3
addFreeVariablesInExpr (TypeInference.AbstractHaskell.Tuple a tyanno exprs)         =
  do
    newExprs <- mapM addFreeVariablesInExpr exprs
    return $ TypeInference.AbstractHaskell.Tuple a tyanno exprs
addFreeVariablesInExpr (TypeInference.AbstractHaskell.List a tyanno exprs)          =
  do
    newExprs <- mapM addFreeVariablesInExpr exprs
    return $ TypeInference.AbstractHaskell.List a tyanno exprs
addFreeVariablesInExpr (TypeInference.AbstractHaskell.Lambda a tyanno pats expr)    =
  do
    newExpr <- addFreeVariablesInExpr expr
    return $ TypeInference.AbstractHaskell.Lambda a tyanno pats expr
addFreeVariablesInExpr (TypeInference.AbstractHaskell.Let a tyanno locals expr)     =
  do
    newExpr <- addFreeVariablesInExpr expr
    newLocals <- mapM addFreeVariablesInLocals locals
    return $ TypeInference.AbstractHaskell.Let a tyanno newLocals newExpr
addFreeVariablesInExpr (DoExpr a tyanno stmts)                                      =
  do
    newStmts <- mapM addFreeVariablesInStmt stmts
    return $ DoExpr a tyanno stmts
addFreeVariablesInExpr (TypeInference.AbstractHaskell.ListComp a tyanno expr stmts) =
  do
    newExpr <- addFreeVariablesInExpr expr
    newStmts <- mapM addFreeVariablesInStmt stmts
    return $ TypeInference.AbstractHaskell.ListComp a tyanno newExpr newStmts

addFreeVariablesInStmt :: MonadState LambdaState m => Statement l -> m (Statement l)
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

addFreeVariablesInBExprs :: MonadState LambdaState m => BranchExpr l -> m (BranchExpr l)
addFreeVariablesInBExprs (Branch a pat expr) =
  do
    newExpr <- addFreeVariablesInExpr expr
    return $ Branch a pat newExpr

addFreeVariablesInLocals :: MonadState LambdaState m => LocalDecl l -> m (LocalDecl l)
addFreeVariablesInLocals (LocalFunc funcdecl) =
  do
    newfunc <- addFreeVariablesAsParametersForFuncDecl funcdecl
    return $ newfunc
addFreeVariablesInLocals (LocalPat a pat expr locals) =
  do
    newLocals <- mapM addFreeVariablesInLocals locals
    newExpr <- addFreeVariablesInExpr expr
    return $ LocalPat a pat newExpr newLocals

addFreeVariablesAsParametersForFuncDecl :: MonadState LambdaState m => FuncDecl a -> m (LocalDecl a)
addFreeVariablesAsParametersForFuncDecl (Func a name arity _ t rules) =
  do
   newRules <- findAndAddFreeVariables (snd $ fst name) rules
   newArity <- countNewArity (snd $ fst name) arity
   return $ LocalFunc $ Func a name newArity Public t newRules

addFreeVariablesAsParametersForPattern :: LocalDecl a -> [LocalDecl a]
addFreeVariablesAsParametersForPattern x@(LocalPat l pat expr locals) = do
  let ev  =  extractVarOutOfExpr2 expr
  let f = addToPatterns ev [pat] l
  let t = TypeInference.AbstractHaskell.Rule l NoTypeAnn f (SimpleRhs expr) locals
  let r = Rules [t]
  case length(ev) of
    0 -> return x
    _ -> return $ LocalFunc $ Func l ((parseNamePattern pat),l) (length ev) Public Untyped undefined
    -- hier fehlt noch was ... bei den rules?

parseNamePattern :: Pattern l -> TypeInference.AbstractHaskell.QName
parseNamePattern (TypeInference.AbstractHaskell.PVar t ((i,s),l)) = ("",s)
parseNamePattern (PComb l t (qn,a) pats)                          = qn
parseNamePattern (PAs a t ((i,s),l) pat)                          = ("",s)
parseNamePattern (TypeInference.AbstractHaskell.PLit t (l, a))    = ("", "Literal")
parseNamePattern (TypeInference.AbstractHaskell.PTuple a t pats)  = ("","Tupel")
parseNamePattern (TypeInference.AbstractHaskell.PList a t pats)   = ("","List")

extractVarOutOfExpr2 :: Expr l -> [VarName]
extractVarOutOfExpr2 (TypeInference.AbstractHaskell.Var t (v,l))            = [v]
extractVarOutOfExpr2 (Apply l t expr1 expr2)                                =
  extractVarOutOfExpr2 expr1 ++ extractVarOutOfExpr2 expr2
extractVarOutOfExpr2 (InfixApply l t expr1 n expr2)                         =
  extractVarOutOfExpr2 expr1 ++ extractVarOutOfExpr2 expr2
extractVarOutOfExpr2 (TypeInference.AbstractHaskell.Lambda l t pats expr)   =
   findDifference (extractVars pats)(extractVarOutOfExpr2 expr)
extractVarOutOfExpr2 (TypeInference.AbstractHaskell.Let l t locals expr)    =
  extractVarOutOfExpr2 expr ++ extractVarsOutOfLocals locals
extractVarOutOfExpr2 (IfThenElse l t expr1 expr2 expr3)                     =
  extractVarOutOfExpr2 expr1 ++ extractVarOutOfExpr2 expr2 ++ extractVarOutOfExpr2 expr3
extractVarOutOfExpr2 (TypeInference.AbstractHaskell.List l t exprs)         =
  concatMap extractVarOutOfExpr2 exprs
extractVarOutOfExpr2 (TypeInference.AbstractHaskell.Tuple l t exprs)        =
  concatMap extractVarOutOfExpr2 exprs
extractVarOutOfExpr2 (Typed l t expr te)                                    =
  extractVarOutOfExpr2 expr
extractVarOutOfExpr2 (TypeInference.AbstractHaskell.Case l t expr branches) =
  extractVarOutOfExpr2 expr ++ concatMap extractVarOutOfBranches branches
extractVarOutOfExpr2 (DoExpr l t stmts)                                     =
  extractVarOutOfStmts stmts
extractVarOutOfExpr2 (TypeInference.AbstractHaskell.ListComp l t expr1 stm) =
  extractVarOutOfExpr2 expr1 ++ extractVarOutOfStmts stm
extractVarOutOfExpr2 _                                                      = []

countNewArity :: MonadState LambdaState m => String -> Arity -> m Arity
countNewArity name arity =
  do
    frees <- getCountOfFrees name
    return $ arity + frees

findAndAddFreeVariables :: MonadState LambdaState m => String -> Rules l -> m (Rules l)
findAndAddFreeVariables name (Rules rls) = do
  rules <- mapM (findAndAddFreeVariablesRules name) rls
  return $ Rules rules

findAndAddFreeVariablesRules :: MonadState LambdaState m => String -> TypeInference.AbstractHaskell.Rule l ->  m (TypeInference.AbstractHaskell.Rule l)
findAndAddFreeVariablesRules name (TypeInference.AbstractHaskell.Rule l t pats rhs locals) = do
   let varExtrleft = extractVars pats
   let varExtrright = extractRhsVars rhs
   let varsToAdd = findDifference varExtrleft varExtrright
   insertFrees name varsToAdd
   let va = (addToPatterns (reverse varsToAdd) pats l)
   let (_:js)=va
   return $ TypeInference.AbstractHaskell.Rule l t va rhs locals

addToPatterns :: [VarName] -> [Pattern l] -> l -> [Pattern l]
addToPatterns []     ys l = ys
addToPatterns (x:xs) ys l = addToPatterns xs (ys ++ [(TypeInference.AbstractHaskell.PVar NoTypeAnn (x,l))]) l

insertFrees :: MonadState LambdaState m => String -> [VarName] -> m ()
insertFrees name xs = do
   ldb <- get
   case Data.Map.Lazy.lookup name (locals ldb) of
     Nothing -> put $ LambdaState {frees = insert name xs (frees ldb), locals = insert name (length xs) $ locals ldb}
     Just x  -> put $ LambdaState {frees = insert name xs (frees ldb), locals = insert name (x +(length xs)) (locals ldb)}

findDifference :: [VarName] -> [VarName] -> [VarName]
findDifference xs ys = [y | y <- ys, not(elem y xs)]

extractVars :: [Pattern l] -> [VarName]
extractVars []                                                  = []
extractVars (x@(TypeInference.AbstractHaskell.PVar t (v,l)):xs) = v : extractVars xs
extractVars ((PComb l t name pats):xs)                          = extractVars pats ++ extractVars xs
extractVars ((PAs l t name pat):xs)                             = extractVars [pat] ++ extractVars xs
extractVars ((TypeInference.AbstractHaskell.PTuple l t pat):xs) = extractVars pat ++ extractVars xs
extractVars ((TypeInference.AbstractHaskell.PList l t pat):xs)  = extractVars pat ++ extractVars xs
extractVars (x:xs)                                              = extractVars xs

extractRhsVars :: TypeInference.AbstractHaskell.Rhs l -> [VarName]
extractRhsVars (SimpleRhs expr)   = extractVarOutOfExpr expr
extractRhsVars (TypeInference.AbstractHaskell.GuardedRhs _ exprs) = concatMap extractVarOutOfExpr (makeList exprs)

makeList :: [(Expr l, Expr l)] -> [Expr l]
makeList []          = []
makeList ((x,y):xys) = [x,y] ++ makeList xys

extractVarOutOfExpr :: Expr l -> [VarName]
extractVarOutOfExpr (TypeInference.AbstractHaskell.Var t (v,l))            = [v]
extractVarOutOfExpr (Apply l t expr1 expr2)                                =
  extractVarOutOfExpr expr1 ++ extractVarOutOfExpr expr2
extractVarOutOfExpr (InfixApply l t expr1 n expr2)                         =
  extractVarOutOfExpr expr1 ++ extractVarOutOfExpr expr2
extractVarOutOfExpr (TypeInference.AbstractHaskell.Lambda l t pats expr)   =
  extractVars pats ++ extractVarOutOfExpr expr
extractVarOutOfExpr (TypeInference.AbstractHaskell.Let l t locals expr)    =
  extractVarOutOfExpr expr ++ extractVarsOutOfLocals locals
extractVarOutOfExpr (IfThenElse l t expr1 expr2 expr3)                     =
  extractVarOutOfExpr expr1 ++ extractVarOutOfExpr expr2 ++ extractVarOutOfExpr expr3
extractVarOutOfExpr (TypeInference.AbstractHaskell.List l t exprs)         =
  concatMap extractVarOutOfExpr exprs
extractVarOutOfExpr (TypeInference.AbstractHaskell.Tuple l t exprs)        =
  concatMap extractVarOutOfExpr exprs
extractVarOutOfExpr (Typed l t expr te)                                    =
  extractVarOutOfExpr expr
extractVarOutOfExpr (TypeInference.AbstractHaskell.Case l t expr branches) =
  extractVarOutOfExpr expr ++ concatMap extractVarOutOfBranches branches
extractVarOutOfExpr (DoExpr l t stmts)                                     =
  extractVarOutOfStmts stmts
extractVarOutOfExpr (TypeInference.AbstractHaskell.ListComp l t expr1 stm) =
  extractVarOutOfExpr expr1 ++ extractVarOutOfStmts stm
extractVarOutOfExpr _                                                      = []

extractVarOutOfStmts :: [Statement l] -> [VarName]
extractVarOutOfStmts []                     = []
extractVarOutOfStmts ((SExpr expr):xs)      = extractVarOutOfExpr expr ++ extractVarOutOfStmts xs
extractVarOutOfStmts ((SPat l pat expr):xs) = extractVars [pat] ++ extractVarOutOfExpr expr ++ extractVarOutOfStmts xs
extractVarOutOfStmts ((SLet l locals):xs)   = extractVarsOutOfLocals locals

extractVarsOutOfLocals :: [LocalDecl l] -> [VarName]
extractVarsOutOfLocals []                   = []
extractVarsOutOfLocals ((LocalFunc funcdecl):xs) =
  extractVarsOutOfFuncDecl funcdecl ++ extractVarsOutOfLocals xs
extractVarsOutOfLocals ((LocalPat a pat expr locals):xs) =
  extractVarOutOfExpr expr ++ extractVarsOutOfLocals locals ++ extractVarsOutOfLocals xs

extractVarsOutOfFuncDecl :: FuncDecl l -> [VarName]
extractVarsOutOfFuncDecl (Func a name arity visibility tsig rules) = extractVarOutOfRules rules

extractVarOutOfRules :: Rules l -> [VarName]
extractVarOutOfRules (Rules xs) = extractVarOutOfRule xs

extractVarOutOfRule :: [TypeInference.AbstractHaskell.Rule l] -> [VarName]
extractVarOutOfRule [] = []
extractVarOutOfRule ((TypeInference.AbstractHaskell.Rule a tyanno pats rhs locals):xs) =
  extractVarOutOfRhs rhs ++ extractVarsOutOfLocals locals ++ extractVarOutOfRule xs

extractVarOutOfRhs :: TypeInference.AbstractHaskell.Rhs l -> [VarName]
extractVarOutOfRhs (SimpleRhs expr) = extractVarOutOfExpr expr
extractVarOutOfRhs (TypeInference.AbstractHaskell.GuardedRhs a es) = extractVarOutOfTups es

extractVarOutOfTups :: [(Expr l, Expr l)] -> [VarName]
extractVarOutOfTups []     = []
extractVarOutOfTups (e:es) =
  extractVarOutOfExpr (fst e) ++ extractVarOutOfExpr (snd e) ++ extractVarOutOfTups es

extractVarOutOfBranches :: BranchExpr l -> [VarName]
extractVarOutOfBranches (Branch l pat expr) = extractVars [pat] ++ extractVarOutOfExpr expr

transFormLocalProg :: [FuncDecl l] -> Prog l -> [FuncDecl l]
transFormLocalProg list (Prog n x y fundecls) =
  list ++ (concatMap (transFormLocalFuncDecl list) fundecls)

transFormLocalFuncDecl :: [FuncDecl l] -> FuncDecl l -> [FuncDecl l]
transFormLocalFuncDecl list (Func x y z a b rules) =
  list ++ transFormLocalRules list rules

transFormLocalRules :: [FuncDecl l] -> Rules l -> [FuncDecl l]
transFormLocalRules list (Rules rule) = do
  list ++ (concatMap (transFormLocalRule list) rule)

transFormLocalRule :: [FuncDecl l] -> TypeInference.AbstractHaskell.Rule l -> [FuncDecl l]
transFormLocalRule list (TypeInference.AbstractHaskell.Rule a b c d e) =
  list ++ transFormLocalRhs list d ++ (concatMap transFormLocal e)

transFormLocalRhs :: [FuncDecl l] -> TypeInference.AbstractHaskell.Rhs l -> [FuncDecl l]
transFormLocalRhs list (SimpleRhs expr) =
  list ++ transFormLocalExpr list expr
transFormLocalRhs list (TypeInference.AbstractHaskell.GuardedRhs a exprs) =
  list ++ (concatMap (transFormLocalListExpr list) exprs)

transFormLocalListExpr :: [FuncDecl l] -> (Expr l, Expr l) -> [FuncDecl l]
transFormLocalListExpr list (a,b) =
  list ++ transFormLocalExpr list a ++  transFormLocalExpr list b

transFormLocalExpr :: [FuncDecl l] -> Expr l -> [FuncDecl l]
transFormLocalExpr list x@(TypeInference.AbstractHaskell.Var _ _)                 = list
transFormLocalExpr list x@(TypeInference.AbstractHaskell.Lit _ _)                 = list
transFormLocalExpr list x@(TypeInference.AbstractHaskell.Symbol _ _)              = list
transFormLocalExpr list (Apply a tyanno expr1 expr2)                              =
  list ++ transFormLocalExpr list expr1 ++ transFormLocalExpr list expr2
transFormLocalExpr list (InfixApply a tyanno expr1 name expr2)                    =
  list ++ transFormLocalExpr list expr1 ++ transFormLocalExpr list expr2
transFormLocalExpr list (TypeInference.AbstractHaskell.Case a tyanno expr bexprs) =
  list ++ transFormLocalExpr list expr ++ (concatMap (transFormLocalExprBranches list) bexprs)
transFormLocalExpr list (Typed a tyanno expr texpr)                               =
  list ++ transFormLocalExpr list expr
transFormLocalExpr list (IfThenElse a tyanno expr1 expr2 expr3)                   =
  list ++ transFormLocalExpr list expr1 ++ transFormLocalExpr list expr2 ++ transFormLocalExpr list expr3
transFormLocalExpr list (TypeInference.AbstractHaskell.Tuple a tyanno exprs)      =
  list ++ (concatMap (transFormLocalExpr list) exprs)
transFormLocalExpr list (TypeInference.AbstractHaskell.List a tyanno exprs)       =
  list ++ (concatMap (transFormLocalExpr list) exprs)
transFormLocalExpr list (TypeInference.AbstractHaskell.Lambda a tyanno pats expr) =
  list ++ transFormLocalExpr list expr
transFormLocalExpr list (TypeInference.AbstractHaskell.Let a tyanno locals expr)  =
  list ++ transFormLocalExpr list expr ++ (concatMap transFormLocal locals)
transFormLocalExpr list (DoExpr a tyanno stmts)                                   =
  list ++ (concatMap (transFormLocalStmt list) stmts)
transFormLocalExpr list (TypeInference.AbstractHaskell.ListComp a tyanno expr stmts) =
  list ++ transFormLocalExpr list expr ++ (concatMap (transFormLocalStmt list) stmts)

transFormLocalStmt :: [FuncDecl l] -> Statement l -> [FuncDecl l]
transFormLocalStmt list (SExpr expr)      =
  list ++ transFormLocalExpr list expr
transFormLocalStmt list (SPat a pat expr) =
  list ++ transFormLocalExpr list expr
transFormLocalStmt list (SLet a locals)   =
  list ++ (concatMap transFormLocal locals)

transFormLocalExprBranches :: [FuncDecl l] -> BranchExpr l -> [FuncDecl l]
transFormLocalExprBranches list (Branch a pat expr) =
  list ++ transFormLocalExpr list expr

transFormLocal :: LocalDecl l -> [FuncDecl l]
transFormLocal (LocalFunc fd) = [fd]
--transFormLocal (LocalPat l pat expr lcs) = undefined
--  [Func l (("",""),l) undefined Public Untyped (Rules [TypeInference.AbstractHaskell.Rule l NoTypeAnn [pat] rhs []])]
--  ++ (concatMap transFormLocal lcs)--
--mit expr und nicht mit rhs

-------------------------------------------------------------------------------------------------------------------------------------------------------
-- STATE ----------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------
data AHState = AHState { idx :: Int
                       , vmap :: Map String Int
                       }

initialState = AHState 0 empty

getidx :: MonadState AHState m => String -> m Int
getidx name = do
    ahs <- get
    case Data.Map.Lazy.lookup name (vmap ahs) of
      Just x -> return x
      Nothing -> do
                   let idx' = idx ahs
                   put $ AHState {idx= idx' +1 , vmap=insert name idx' $ vmap ahs}
                   return idx'
--------------------------------------------------------------------------------------------------------------------------------------------------------
-- AST UMFORMEN IN ABSTACTHASKELL ----------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------

astToAH :: Module a -> Prog a
astToAH modu = evalState (astToAbstractHaskell modu) initialState

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

filterFunDecls :: [Decl l] -> [Decl l]
filterFunDecls []                                 = []
filterFunDecls (x@(FunBind l mas@(m:matches)):xs) = x:filterFunDecls xs
filterFunDecls (x@(PatBind _ _ _ _):xs)           = x:filterFunDecls xs
filterFunDecls (x:xs)                             = filterFunDecls xs

filterdecls :: [Decl l] -> [Decl l]
filterdecls []                                             = []
filterdecls (x@(Language.Haskell.Exts.TypeDecl _ _ _):xs)  = x : filterdecls xs
filterdecls (x@(DataDecl _ (DataType _) Nothing _ _ _):xs) = x : filterdecls xs
filterdecls (x:xs)                                         = filterdecls xs
---------------------------------------------------------------------------------------------------------------------------------------------------------
-- ERSTELLUNG VON TYPEDECLS -----------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------

parseTypDecls :: MonadState AHState m => MName -> Decl l -> m (TypeDecl l)
parseTypDecls str (Language.Haskell.Exts.TypeDecl l declhead typ)               =
   do
     t <- parseTyp str typ
     tv <- parseTypeVariables typ
     return $ TypeSyn l (parseTypeName str declhead,l) Public tv t
parseTypDecls str (DataDecl l (DataType _) Nothing declhead qualcondecls mderv) =
  do
    etv <- mapM  extractTypvariables $ filterqual qualcondecls
    let he = concat etv
    qcd <- mapM (parseQualConDecl str) qualcondecls
    return $ Type l (parseTypeName str declhead,l) Public he qcd

filterqual :: [QualConDecl l] -> [QualConDecl l]
filterqual []                                    = []
filterqual (x@(QualConDecl _ (Just tvb) _ _):xs) = x : filterqual xs
filterqual (x:xs)                                = filterqual xs

extractTypvariables :: MonadState AHState m => QualConDecl l -> m [(VarName, l)]
extractTypvariables (QualConDecl _ (Just tvb) _ _) = do
                                                      tvb <- mapM parseTVB tvb
                                                      return tvb

parseTVB :: MonadState AHState m => TyVarBind l -> m (VarName, l)
parseTVB (KindedVar l name _) = do
                                  y <- getidx (parsename name)
                                  return ((y, parsename name),l)
parseTVB (UnkindedVar l name) = do
                                  y <- getidx (parsename name)
                                  return ((y, parsename name),l)

parseQualConDecl :: MonadState AHState m => MName -> QualConDecl l -> m (ConsDecl l)
parseQualConDecl str (QualConDecl _ _ _ conDecl) =
  do
    cd <- parseConDecl str $ conDecl
    return cd

parseConDecl :: MonadState AHState m => MName -> ConDecl l -> m (ConsDecl l)
parseConDecl str (ConDecl l name typs)       =
  do
    tps <- mapM (parseTyp str) typs
    return $ TypeInference.AbstractHaskell.Cons l ((str,parsename name),l) (length typs) Public tps
parseConDecl str (InfixConDecl l t1 name t2) =
  do
    tp1 <- parseTyp str t1
    tp2 <- parseTyp str t2
    let tp = [tp1] ++ [tp2]
    return $ TypeInference.AbstractHaskell.Cons l ((str,parsename name),l) (length tp) Public tp
parseConDecl _  _                            = error "parseConDecl"

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
parseTypeVariables (TyList _ t)        = do
                                           pv <- parseTypeVariables t
                                           return pv
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

parseTypeName :: String -> DeclHead l -> TypeInference.AbstractHaskell.QName
parseTypeName mn (DHead l name)     = (mn,parsename name)
parseTypeName mn (DHInfix l _ name) = (mn,parsename name)
parseTypeName mn (DHParen l dh)     = parseTypeName mn dh
parseTypeName mn (DHApp l dh _)     = parseTypeName mn dh

---------------------------------------------------------------------------------------------------------------------------------------------------------
-- ERSTELLUNG VON FUNCDECLS -----------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------

parseFunDecls :: MonadState AHState m => MName -> [(Name l, Type l)] -> Decl l -> m (FuncDecl l)
parseFunDecls modu ts (FunBind l mas@(m:matches)) =
   do
     let fn = parseMatchName m
     put $ AHState {idx = 0 , vmap = empty}
     btd <- buildType l modu (searchForType fn ts)
     put $ AHState {idx = 0, vmap = empty}
     rl <- mapM (parseRules modu ts) mas
     return $ Func l ((modu,fn),l) (parseArity mas) Public btd (Rules rl)
parseFunDecls modu ts (PatBind l pat rhs mbinds) =
  do
    let pn = parseNameOutOfPattern pat
    rl <- parseRulesOutOfPats modu ts pat rhs
    btd <- buildType l modu (searchForType pn ts)
    return $ Func l ((modu,pn),l) 0 Public btd rl
parseFunDecls modulename typsignatures _ = return $ Func undefined ((modulename,""), undefined) 0 Public Untyped (Rules [])

parseRulesOutOfPats :: MonadState AHState m => MName -> [(Name a, Type a)] -> Pat a -> Language.Haskell.Exts.Rhs a -> m (Rules a)
parseRulesOutOfPats  modu ts pat rhs  =
  do
    rs <- parseRule modu ts [pat] rhs
    return $ Rules [rs]

parseNameOutOfPattern :: Pat l -> String
parseNameOutOfPattern (Language.Haskell.Exts.PVar l name)         =
  parsename name
parseNameOutOfPattern (Language.Haskell.Exts.PTuple l Boxed pats) =
  "Tupel " ++ (concatMap parseNameOutOfPattern pats)
parseNameOutOfPattern (Language.Haskell.Exts.PList l pats)        =
  "Liste " ++ (concatMap parseNameOutOfPattern pats)
parseNameOutOfPattern (PParen _ pat)                              =
  parseNameOutOfPattern pat
parseNameOutOfPattern (PAsPat l name pat)                         =
  parsename name

parseRules :: MonadState AHState m => MName -> [(Name a, Type a)] -> Match a -> m (TypeInference.AbstractHaskell.Rule a)
parseRules str t (Match l _ pats rhs _)         = do
                                                    r <- parseRule str t pats rhs
                                                    return r
parseRules _ t (InfixMatch _ pat1 _ pats rhs _) = error "parseRules"

parseRule :: MonadState AHState m => MName -> [(Name a, Type a)] -> [Pat a] -> Language.Haskell.Exts.Rhs a -> m (TypeInference.AbstractHaskell.Rule a)
parseRule str t pats (UnGuardedRhs l expr)  = do
                                                patt <- mapM (parsePatterns str) pats
                                                ep <- parseExpr str t expr
                                                bnd <- parseLocal str t expr
                                                return $ TypeInference.AbstractHaskell.Rule l NoTypeAnn patt (SimpleRhs ep) bnd
parseRule str t pats (GuardedRhss l gurhss) =
  do
    patt <- mapM (parsePatterns str) pats
    g <- mapM (parseGuarded str t) gurhss
    return $ TypeInference.AbstractHaskell.Rule l NoTypeAnn patt (TypeInference.AbstractHaskell.GuardedRhs l (concat g)) []

parseGuarded :: MonadState AHState m => MName -> [(Name l, Type l)] -> GuardedRhs l -> m [(Expr l, Expr l)]
parseGuarded str t (Language.Haskell.Exts.GuardedRhs l stmts expr) =
  do
    splt <- mapM (splitStatments2 str t expr) stmts
    return splt

splitStatments2 :: MonadState AHState m => MName -> [(Name l, Type l)] -> Exp l -> Stmt l -> m (Expr l, Expr l)
splitStatments2 str t expr stmts =
  do
    expr2 <- parseExpr str t expr
    expr1 <- parseStmts2 str t stmts
    return (expr1,expr2)

filterStmts :: [Stmt a] -> [Stmt a]
filterStmts []                        = []
filterStmts (x@(Qualifier l expr):xs) = x : filterStmts xs
filterStmts (x:xs)                    = filterStmts xs

parseStmts2 :: MonadState AHState m => MName -> [(Name a, Type a)] -> Stmt a -> m (Expr a)
parseStmts2 str t (Qualifier l expr) = parseExpr str t expr

parseFuncPatDecls :: MonadState AHState m => String -> [(Name a, Type a)] -> [Decl a] -> m [LocalDecl a]
parseFuncPatDecls _ _ []                                      = return []
parseFuncPatDecls str t ((s@(FunBind l matches)):xs)          = do
                                                                  fd <- parseFunDecls str t s
                                                                  return $ [LocalFunc fd]
parseFuncPatDecls str t ((PatBind l pat rhs@(UnGuardedRhs a expr) (Just wbind)):xs) =
   do
    rh <- parseExprOutOfRhs str t rhs
    patt <- parsePatterns str pat
    bnd <- parseBinds str t wbind
    return $ [LocalPat l patt rh bnd]
--parseFuncPatDecls str t ((PatBind l pat rhs@(GuardedRhss a gds) (Just wbind)):xs) |length gds <= 1 =
--   do
--     rh <- parseExprOutOfGrd str t (head gds)
--     patt <- parsePatterns str pat
--     bnd <- parseBinds str t wbind
--     return $ [LocalPat l patt rh bnd]
--                                                                                  |otherwise      =
--   do
--     let name = parseNameOutOfPattern pat
--     rh <- parseExprOutOfGrd str t (head gds)
--     patt <- parsePatterns str pat
--     bnd <- parseBinds str t wbind
--     rules <- mapM (parseRulesOutOfGuarded str t) gds
--     rulesR <- makeRules rules
--     return $ [LocalFunc $ Func l (("",name),l) 0 Public Untyped (Rules rulesR)]
parseFuncPatDecls _  _  _                                     =  return []

--makeRules []     = []
--makeRules (x:xs) =

--parseRulesOutOfGuarded ::MonadState AHState m => MName -> [(Name a, Type a)] -> Language.Haskell.Exts.GuardedRhs l -> m [(Expr l, Expr l)]
parseRulesOutOfGuarded str t (Language.Haskell.Exts.GuardedRhs l stmts expr) = do
  tups <- mapM (parseTupels str t expr) stmts
  return $ TypeInference.AbstractHaskell.GuardedRhs l tups
--  parseTupels str t stmts expr

parseTupels :: MonadState AHState m => MName -> [(Name a, Type a)] ->  Exp a -> Stmt a -> m (Expr a, Expr a)
parseTupels str t expr2 (Qualifier l expr1) = do
   ex1 <- parseExpr str t expr1
   ex2 <- parseExpr str t expr2
   return $ (ex1,ex2)

parseExprOutOfRhs :: MonadState AHState m => MName -> [(Name a, Type a)] -> Language.Haskell.Exts.Rhs a -> m (Expr a)
parseExprOutOfRhs str t (UnGuardedRhs l expr) =
  do
    expr1 <- parseExpr str t expr
    return expr1

parseExprOutOfGrd :: MonadState AHState m => MName -> [(Name a, Type a)] -> GuardedRhs a -> m (Expr a)
parseExprOutOfGrd str t (Language.Haskell.Exts.GuardedRhs l stmts expr) = parseExpr str t expr

parseRigthHands :: MonadState AHState m => MName -> [(Name a, Type a)] -> Language.Haskell.Exts.Rhs a -> m (TypeInference.AbstractHaskell.Rhs a)
parseRigthHands str t (UnGuardedRhs l e)   = do
                                               expr <- parseExpr str t e
                                               return $ SimpleRhs expr
parseRigthHands str t (GuardedRhss l grhs) = do
                                               gud <- mapM (parseGuarded str t) grhs
                                               return $ TypeInference.AbstractHaskell.GuardedRhs l (concat gud)

parseLocal :: MonadState AHState m => String -> [(Name a, Type a)] -> Exp a -> m [LocalDecl a]
parseLocal str t(Language.Haskell.Exts.Let l binds e) = do
                                                          bnd <- parseBinds str t binds
                                                          return bnd
parseLocal _ _ _                = return []

parseExpr :: MonadState AHState m => MName -> [(Name a, Type a)] -> Exp a -> m (Expr a)
parseExpr _  _ (Language.Haskell.Exts.Var l qn)         = do
                                                           y <- getidx (parseQName qn)
                                                           return $ TypeInference.AbstractHaskell.Var NoTypeAnn ((y,parseQName qn),l)
parseExpr mn _ (Con l qn)                               = return $ TypeInference.AbstractHaskell.Symbol NoTypeAnn  ((mn,parseQName qn), l)
parseExpr _  _ (Language.Haskell.Exts.Lit l lit)        = return $ TypeInference.AbstractHaskell.Lit NoTypeAnn (parseLiteral lit, l)
parseExpr mn t (InfixApp l exp1 qop exp2)               = do
                                                            expr1 <- parseExpr mn t exp1
                                                            expr2 <- parseExpr mn t exp2
                                                            return $ InfixApply l NoTypeAnn expr1 (parseQOp mn qop, l) expr2
parseExpr mn t (Language.Haskell.Exts.Lambda l pats e)  = do
                                                            expr <- parseExpr mn t e
                                                            pat <- mapM (parsePatterns mn) pats
                                                            return $ TypeInference.AbstractHaskell.Lambda l NoTypeAnn pat expr
parseExpr mn t (Language.Haskell.Exts.Let l binds e)    = do
                                                           expr <- parseExpr mn t e
                                                           bnd <- parseBinds mn t binds
                                                           return $ TypeInference.AbstractHaskell.Let l NoTypeAnn bnd expr
parseExpr mn t (If l e1 e2 e3)                          = do
                                                            expr1 <- parseExpr mn t e1
                                                            expr2 <- parseExpr mn t e2
                                                            expr3 <- parseExpr mn t e3
                                                            return $ IfThenElse l NoTypeAnn (expr1) (expr2) (expr3)
parseExpr mn t (Language.Haskell.Exts.Case l e alters)  = do
                                                           expr <- parseExpr mn t e
                                                           alt <- mapM (parseAlternatives mn t) alters
                                                           return $ TypeInference.AbstractHaskell.Case l NoTypeAnn expr alt
parseExpr mn t (Do  l stms)                             = do
                                                           st <- mapM (parseStms mn t) stms
                                                           return $ DoExpr l NoTypeAnn st
parseExpr mn t (Language.Haskell.Exts.Tuple l Boxed es) = do
                                                           e <- mapM (parseExpr mn t) es
                                                           return $ TypeInference.AbstractHaskell.Tuple l NoTypeAnn e
parseExpr mn t (Language.Haskell.Exts.List l exprs)     = do
                                                            eps <- mapM (parseExpr mn t) exprs
                                                            return $ TypeInference.AbstractHaskell.List l NoTypeAnn eps
parseExpr mn t (Paren _ e)                              = do
                                                            expr <- parseExpr mn t e
                                                            return expr
parseExpr mn t (App l e1 e2)                            = do
                                                            expr1 <- parseExpr mn t e1
                                                            expr2 <- parseExpr mn t e2
                                                            return $ Apply l NoTypeAnn expr1 expr2
parseExpr mn t (Language.Haskell.Exts.ListComp l e qs)  = do
                                                            expr <- parseExpr mn t e
                                                            let r = filterQualsStmts qs
                                                            q <- mapM (parseQualsStms mn t) r
                                                            return $ TypeInference.AbstractHaskell.ListComp l NoTypeAnn expr q
parseExpr mn t (ExpTypeSig l expr typ)                  = do
                                                            e <- parseExpr mn t expr
                                                            t <- parseTyp mn typ
                                                            return $ Typed l NoTypeAnn e t
parseExpr mn t (EnumFrom l expr)                        = do
                                                            e <- parseExpr mn t expr
                                                            return $ Apply l NoTypeAnn (TypeInference.AbstractHaskell.Lit NoTypeAnn (Stringc "enumFrom", l)) e
parseExpr mn t (EnumFromTo l exp1 exp2)                 = do
                                                            expr1 <- parseExpr mn t exp1
                                                            expr2 <- parseExpr mn t exp2
                                                            return $ Apply l NoTypeAnn (TypeInference.AbstractHaskell.Lit NoTypeAnn (Stringc "enumFromTo",l))
                                                                     (Apply l NoTypeAnn expr1 expr2)
parseExpr mn t (EnumFromThen l exp1 exp2)               = do
                                                            expr1 <- parseExpr mn t exp1
                                                            expr2 <- parseExpr mn t exp2
                                                            return $ Apply l NoTypeAnn (TypeInference.AbstractHaskell.Lit NoTypeAnn (Stringc "enumFromThen",l))
                                                                     (Apply l NoTypeAnn expr1 expr2)
parseExpr mn t (EnumFromThenTo l exp1 exp2 exp3)        = do
                                                            expr1 <- parseExpr mn t exp1
                                                            expr2 <- parseExpr mn t exp2
                                                            expr3 <- parseExpr mn t exp3
                                                            return $ Apply l NoTypeAnn (TypeInference.AbstractHaskell.Lit NoTypeAnn (Stringc "enumFromThenTo",l))
                                                                     (Apply l NoTypeAnn  expr1 (Apply l NoTypeAnn expr2 expr3))
parseExpr _  _ _                                        = error "parseExpr"

rightHandtoExp :: MonadState AHState m => MName -> [(Name a, Type a)] -> Language.Haskell.Exts.Rhs a -> m (Expr a)
rightHandtoExp str t (UnGuardedRhs _ e) = do
                                            expr <- parseExpr str t e
                                            return expr
rightHandtoExp str t (GuardedRhss _ gdrhs) = error "rightHandtoExp"

parsePatterns:: MonadState AHState m => MName -> Pat l -> m (Pattern l)
parsePatterns mn (Language.Haskell.Exts.PVar l name)         = do
                                                                 y <- getidx (parsename name)
                                                                 return $ TypeInference.AbstractHaskell.PVar NoTypeAnn ((y,parsename name),l)
parsePatterns mn (Language.Haskell.Exts.PLit l sign lit)     = return $ TypeInference.AbstractHaskell.PLit NoTypeAnn (parseLiteral lit, l)
parsePatterns mn (PApp l qn pats)                            = do
                                                                pat <- mapM (parsePatterns mn) pats
                                                                return $ PComb l NoTypeAnn ((mn,parseQName qn),l) pat
parsePatterns mn (Language.Haskell.Exts.PTuple l Boxed pats) = do
                                                                 pat <- mapM (parsePatterns mn) pats
                                                                 return $ TypeInference.AbstractHaskell.PTuple l NoTypeAnn pat
parsePatterns mn (Language.Haskell.Exts.PList l pats)        = do
                                                                 pat <- mapM (parsePatterns mn) pats
                                                                 return $ TypeInference.AbstractHaskell.PList l NoTypeAnn pat
parsePatterns mn (PParen _ pat)                              = do
                                                                 patt <- parsePatterns mn pat
                                                                 return patt
parsePatterns mn (PAsPat l name pat)                         = do
                                                                 y <- getidx (parsename name)
                                                                 patt <- parsePatterns mn pat
                                                                 return $ PAs l NoTypeAnn ((y,parsename name),l) patt
parsePatterns _  _                                           = error "parsePatterns"

parseStms :: MonadState AHState m => MName -> [(Name a, Type a)] -> Stmt a -> m (Statement a)
parseStms str t (Generator l pat e) = do
                                        patt <- parsePatterns str pat
                                        expr <- parseExpr str t e
                                        return $ SPat l  patt expr
parseStms str t (Qualifier l e)     = do
                                        expr <- parseExpr str t e
                                        return $ SExpr expr
parseStms str t (LetStmt l binds)   = do
                                        bnd <- parseBinds str t binds
                                        return $ SLet l bnd
parseStms _  _ _                    = error "parseStms"

parseBinds :: MonadState AHState m => String -> [(Name a, Type a)] -> Binds a -> m [LocalDecl a]
parseBinds str t (BDecls _ decls) = do
                                       fpd <- parseFuncPatDecls str t decls
                                       return fpd
parseBinds _ _ _  = return []

parseAlternatives :: MonadState AHState m => MName -> [(Name a, Type a)] -> Alt a -> m (BranchExpr a)
parseAlternatives str t (Alt l pat rhs _) = do
                                              patt <- parsePatterns str pat
                                              rh <- rightHandtoExp str t rhs
                                              return $ Branch l patt rh

parseQOp :: String -> QOp l -> TypeInference.AbstractHaskell.QName
parseQOp mn (QVarOp l qn) = (mn,parseQName qn)
parseQOp mn (QConOp l qn) = (mn, parseQName qn)

parseQualsStms :: MonadState AHState m => MName -> [(Name a, Type a)] -> QualStmt a -> m (Statement a)
parseQualsStms str t (QualStmt _ stm ) = do
                                           st <-  parseStms str t stm
                                           return st

filterQualsStmts :: [QualStmt l] -> [QualStmt l]
filterQualsStmts []                       = []
filterQualsStmts (x@(QualStmt _ stm ):xs) = x : filterQualsStmts xs
filterQualsStmts (x:xs)                   = filterQualsStmts xs

parseLiteral :: Language.Haskell.Exts.Literal l -> TypeInference.AbstractHaskell.Literal
parseLiteral (Char _ c _)     = Charc c
parseLiteral (String _ str _) = Stringc str
parseLiteral (Int _ i _)      = Intc $ fromInteger i
parseLiteral (Frac _ r _)     = Floatc $ fromRational r
parseLiteral _ = error "parseLiteral"

parseTyp :: MonadState AHState m => MName -> Type a -> m (TypeExpr a)
parseTyp _    (TyVar l name)          = do
                                          y <- getidx (parsename name)
                                          return $ TVar ((y, parsename name),l)
parseTyp modu (TyFun l t1 t2)         = do
                                          ty1 <- parseTyp modu t1
                                          ty2 <- parseTyp modu t2
                                          return $ FuncType l ty1 ty2
parseTyp modu (TyTuple l Boxed types) = do
                                          ty <- mapM (parseTyp modu) types
                                          return $ TCons l ((modu, "tupel"),l) ty
parseTyp modu (TyList l typ)          = do
                                          ty <- parseTyp modu typ
                                          return $ TCons l ((modu, "list"),l) [(ty)]
parseTyp modu (TyCon l qname)         = return $ TCons l ((modu, parseQName qname),l) []
parseTyp modu (TyParen l t)           = do
                                          ty <- parseTyp modu t
                                          return $ TCons l ((modu, "paren"),l) [(ty)]
parseTyp modu  _                      = error "parseTyp"

parseArity :: [Match l] -> Int
parseArity []                                        = 0
parseArity ((InfixMatch _ _ _ _ _ _):ms)             = 2
parseArity ((Match l name patterns rhs wbinds) : ms) = length patterns

parseImportList :: [ImportDecl l] -> [(MName, l)]
parseImportList [] = []
parseImportList ((ImportDecl l name _ _ _ _ _ _ ):xs) = [(parseModuleName name,l)] ++ parseImportList xs

parseModuleHead :: Maybe (ModuleHead l) -> MName
parseModuleHead Nothing                   = ""
parseModuleHead (Just (ModuleHead l n _ _)) = parseModuleName n

parseModuleName :: ModuleName l -> String
parseModuleName (ModuleName l str) = str

parseDecls :: [Decl l] -> [(Name l,(Type l))]
parseDecls [] = []
parseDecls (x:xs) = parseOneDecl x ++ parseDecls xs

parseOneDecl :: Decl l -> [(Name l,(Type l ))]
parseOneDecl (Language.Haskell.Exts.TypeSig l names t) = concatMap (parseTypeSig t) names
parseOneDecl _ = []

---------------------------------------------------------------------------------------------------------------------------------------------------------
-- ALLGEMEINE FUNKTIONEN --------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------

parseTypeSig :: Type l -> Name l ->[(Name l,(Type l ))]
parseTypeSig typ name = [(name,typ)]

parseTypeSignatur :: Module l -> [(Name l, Type l)]
parseTypeSignatur (Module l mh mp impdec decls) = parseDecls decls
parseTypeSignatur _ = []

parseFile' :: FilePath -> IO (Module SrcSpanInfo)
parseFile' f = do
        (ParseOk ast) <- parseFile f
        return ast

parseMatchName :: Match l -> String
parseMatchName (Match l name patterns rhs wbinds) = parsename name
parseMatchName (InfixMatch l pat1 name pat2 rhs wbinds) = parsename name

parsename :: Name l -> String
parsename (Ident l name)  = name
parsename (Language.Haskell.Exts.Symbol l name) = name

parseQName :: Language.Haskell.Exts.QName l -> String
parseQName (Qual l mdn name) = parsename name
parseQName (UnQual l name)   = parsename name
parseQName _                 = ""

searchForType :: String -> [(Name l, Type l)] -> Type l
searchForType _ [] = error "noType"
searchForType name ((n , t):nts) | name == (parsename n) = t
                                 | otherwise = searchForType name nts

buildType :: MonadState AHState m => a -> MName -> Type a -> m (TypeSig a)
buildType l modname x          = do
                                   t <- parseTyp modname x
                                   return $ TypeInference.AbstractHaskell.TypeSig t

-------------------------------------------------------------------------------------------------------------------------------------------------------
-- ERSTELLUNG VON OPDECLS -----------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------
--parseOpDecls :: String -> Decl l -> [OpDecl l]
--parseOpDecls str (InfixDecl l assoc (Just mInt) ops) = Prelude.map (parseOps str (parseAssoc assoc) mInt) ops
--parseOpDecls str (InfixDecl l assoc Nothing ops)     = Prelude.map (parseOps str (parseAssoc assoc) (-1)) ops
--parseOpDecls _ _                                     = undefined

--parseAssoc :: Assoc l -> AbstractHaskell.Fixity l
--parseAssoc (AssocNone l)  = InfixOp l
--parseAssoc (AssocLeft l)  = InfixlOp l
--parseAssoc (AssocRight l) = InfixrOp l

--parseOps :: String -> AbstractHaskell.Fixity l -> Int -> Op l ->  OpDecl l
--parseOps str fixity mInt (VarOp l name) = Op l (l,str,parsename name) fixity mInt
--parseOps str fixity mInt (ConOp l name) = Op l (l,str,parsename name) fixity mInt
