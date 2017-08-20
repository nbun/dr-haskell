{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypeInference.HSE2AH (hseToAH,preludeToAH,parseFile',hseExpToAHExpr) where

import Control.Monad.State.Lazy
import Data.Functor
import Data.Map.Lazy                 as DML
import Language.Haskell.Exts         as HSE
import TypeInference.AbstractHaskell as AH
import TypeInference.AHAbstract
import TypeInference.AHAddVariables
import TypeInference.HSEConversion
import TypeInference.TypeSig

parseFile' :: FilePath -> IO (Module SrcSpanInfo)
parseFile' f = do
          (ParseOk ast) <- parseFile f
          return ast

-------------------------------------------------------------------------------
-- MAIN FUNCTION --------------------------------------------------------------
-------------------------------------------------------------------------------

-- | Transforms the 'Prelude' into an abstract Haskell representation.
preludeToAH :: Module a -> Prog a
preludeToAH = hseToNLAH DML.empty

-- | The maping contains already known types and functions from the 'Prelude'.
hseToAH :: DML.Map AH.QName (TypeExpr a) -> Module a -> Prog a
hseToAH tenv m = evalState (nlahToAH (hseToNLAH tenv m)) initialStateLambda

-------------------------------------------------------------------------------
-- LAMBDA LIFTING FOR LOCAL DECLARATIONS  -------------------------------------
-------------------------------------------------------------------------------

-- | Executes the lambda lifting in three steps
--   1. adds free variables to local functions as parameters
--   2. builds a abstract representations with new names for the localdecls
--      functions
--   3. lifts the local functions on top level
--nlahToAH :: MonadState LState m => Prog l -> m (Prog l )
nlahToAH p@(Prog m q t fs) =
  do
   p1 <- addFreeVariablesInProg p
   v@(Prog n i t fd) <- abstrProg p1
   let list = transFormLocalProg [] v
   let newProg = Prog n i t (fd ++ list)
   return newProg

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
transFormLocal (LocalFunc (Func a b c _ d e)) = [(Func a b c Public d e)]
transFormLocal (LocalPat l pat expr lcs) =
  [Func l (("",""),l) undefined Public Untyped (Rules [AH.Rule l NoTypeAnn [pat] (SimpleRhs expr) []])]
  ++ (concatMap transFormLocal lcs)

-------------------------------------------------------------------------------
-- HELPING FUNCTIONS ----------------------------------------------------------
-------------------------------------------------------------------------------

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
