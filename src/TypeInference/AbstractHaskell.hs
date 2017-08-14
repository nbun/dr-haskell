{-|
  This library contains data types for representing Haskell programs in Haskell.
-}

module TypeInference.AbstractHaskell
  ( MName, QName, VarName, Arity, TypeExprEq, TypeExprEqs, Visibility (..)
  , Prog (..), TypeDecl (..), ConsDecl (..), TypeExpr (..), TypeSig (..)
  , TypeAnn (..), FuncDecl (..), Rules (..), Rule (..), Rhs (..), LocalDecl (..)
  , Expr (..), Statement (..), Pattern (..), BranchExpr (..), Literal (..)
  , AHOptions (..)
  , varToString, defaultAHOptions, showQName, showVarName, showProg
  , showTypeDecl, showConsDecl, showTypeExpr, showTypeSig, showTypeAnn
  , showFuncDecl, showRules, showRule, showRhs, showExpr, showPattern
  , showBranchExpr, showLiteral
  ) where

import Goodies (indent, list, one, parensIf, tuple, two, vsep)

-- -----------------------------------------------------------------------------
-- Representation of Haskell programs
-- -----------------------------------------------------------------------------

-- | A module name represented as a string.
type MName = String

-- | A qualified name represented as a tuple consisting of a module name and a
--   string.
type QName = (MName, String)

-- | A variable name represented as a tuple consisting of a variable index and a
--   corresponding string representation. The variable index is represented by
--   an integer greater than or equal to zero.
type VarName = (Int, String)

-- | The arity of a function or type constructor represented as an integer
--   greater than or equal to zero.
type Arity = Int

-- | A type expression equation represented as a pair of type expressions and
--   parameterized over the type of annotations.
type TypeExprEq a = (TypeExpr a, TypeExpr a)

-- | Multiple type expression equations represented as a list of type expression
--   equations and parameterized over the type of annotations.
type TypeExprEqs a = [TypeExprEq a]

-- | The visibility of a function, type constructor or type is either private
--   (not exported) or public (exported).
data Visibility = Private | Public
  deriving Show

-- | Representation of a Haskell module consisting of a module name, a list of
--   imported modules, a list of type declarations and a list of function
--   declarations. The entities can be annotated with any data type.
data Prog a = Prog (MName, a) [(MName, a)] [TypeDecl a] [FuncDecl a]
  deriving Show

-- | Representation of an algebraic data type or type synonym declaration. The
--   entities can be annotated with any data type.
data TypeDecl a = Type a (QName, a) Visibility [(VarName, a)] [ConsDecl a]
                | TypeSyn a (QName, a) Visibility [(VarName, a)] (TypeExpr a)
  deriving Show

-- | Representation of a type constructor declaration consisting of a type
--   constructor name, the arity and visibility of the type constructor and a
--   list of argument types. The entities can be annotated with any data type.
data ConsDecl a = Cons a (QName, a) Arity Visibility [TypeExpr a]
  deriving Show

-- | Representation of a type expression. A type expression is either a type
--   variable, a function type or a type constructor application. The entities
--   can be annotated with any data type.
data TypeExpr a = TVar (VarName, a)
                | FuncType a (TypeExpr a) (TypeExpr a)
                | TCons a (QName, a) [TypeExpr a]
  deriving Show

-- | Representation of a type signature for a function. The entities can be
--   annotated with any data type.
data TypeSig a = Untyped
               | TypeSig (TypeExpr a)
  deriving Show

-- | Representation of a type annotation used to annotate the type of
--   expressions. The entities can be annotated with any data type.
data TypeAnn a = NoTypeAnn
               | TypeAnn (TypeExpr a)
  deriving Show

-- | Representation of a function declaration consisting of a function name, the
--   arity and visibility of the function, a type signature and a list of rules.
--   The entities can be annotated with any data type.
data FuncDecl a = Func a (QName, a) Arity Visibility (TypeSig a) (Rules a)
  deriving Show

-- | Rules are either a list of single rules or no rule at all if the function
--   is defined externally. The entities can be annotated with any data type.
data Rules a = Rules [Rule a]
             | External a (TypeAnn a)
  deriving Show

-- | Representation of a function rule consisting of a type annotation, a list
--   of patterns, a right-hand side and a list of local declarations. The
--   entities can be annotated with any data type.
data Rule a = Rule a (TypeAnn a) [Pattern a] (Rhs a) [LocalDecl a]
  deriving Show

-- | Representation of a rules right-hand side as either guarded or unguarded.
--   The entities can be annotated with any data type.
data Rhs a = SimpleRhs (Expr a)
           | GuardedRhs a [(Expr a, Expr a)]
  deriving Show

-- | Representation of local @let@ or @where@ declarations. The entities can be
--   annotated with any data type.
data LocalDecl a = LocalFunc (FuncDecl a)
                 | LocalPat a (Pattern a) (Expr a) [LocalDecl a]
  deriving Show

-- | Representation of expressions. The entities can be annotated with any data
--   type.
data Expr a = Var (TypeAnn a) (VarName, a)
            | Lit (TypeAnn a) (Literal, a)
            | Symbol (TypeAnn a) (QName, a)
            | Apply a (TypeAnn a) (Expr a) (Expr a)
            | InfixApply a (TypeAnn a) (Expr a) (QName, a) (Expr a)
            | Lambda a (TypeAnn a) [Pattern a] (Expr a)
            | Let a (TypeAnn a) [LocalDecl a] (Expr a)
            | DoExpr a (TypeAnn a) [Statement a]
            | ListComp a (TypeAnn a) (Expr a) [Statement a]
            | Case a (TypeAnn a) (Expr a) [BranchExpr a]
            | Typed a (TypeAnn a) (Expr a) (TypeExpr a)
            | IfThenElse a (TypeAnn a) (Expr a) (Expr a) (Expr a)
            | Tuple a (TypeAnn a) [Expr a]
            | List a (TypeAnn a) [Expr a]
  deriving Show

-- | Representation of statements in @do@ expressions or list comprehensions.
--   The entities can be annotated with any data type.
data Statement a = SExpr (Expr a)
                 | SPat a (Pattern a) (Expr a)
                 | SLet a [LocalDecl a]
  deriving Show

-- | Representation of pattern expressions. The entities can be annotated with
--   any data type.
data Pattern a = PVar (TypeAnn a) (VarName, a)
               | PLit (TypeAnn a) (Literal, a)
               | PComb a (TypeAnn a) (QName, a) [Pattern a]
               | PAs a (TypeAnn a) (VarName, a) (Pattern a)
               | PTuple a (TypeAnn a) [Pattern a]
               | PList a (TypeAnn a) [Pattern a]
  deriving Show

-- | Representation of branches in @case@ expressions. The entities can be
--   annotated with any data type.
data BranchExpr a = Branch a (Pattern a) (Expr a)
  deriving Show

-- | Representation of literals occurring in expressions. It is either an
--   integer, a float, a character, or a string constant.
data Literal = Intc Int
             | Floatc Float
             | Charc Char
             | Stringc String
  deriving Show

-- -----------------------------------------------------------------------------
-- Pretty-printing of abstract Haskell data types
-- -----------------------------------------------------------------------------

-- | Transforms a variable into a string representation.
varToString :: Int -> String
varToString v | v >= 0    = if q == 0 then [c] else c : show q
              | otherwise = error err
  where
    (q, r) = divMod v 26
    c = ['a'..'z'] !! r
    err = "Variables can not be represented by negative integers!"

-- | Representation of pretty-printing options for abstract Haskell data types.
data AHOptions = AHOptions { currentModule :: MName
                           , unqModules    :: [MName] }

-- | The default pretty-printing options for abstract Haskell data types.
defaultAHOptions :: AHOptions
defaultAHOptions = AHOptions { currentModule = ""
                             , unqModules    = [] }

-- | Transforms a qualified name into a string representation.
showQName :: AHOptions -> QName -> String
showQName opts (mn, n) | mn == currentModule opts  = n
                       | mn `elem` unqModules opts = n
                       | otherwise                 = mn ++ "." ++ n

-- | Transforms a variable name into a string representation.
showVarName :: VarName -> String
showVarName = snd

-- | Transforms a Haskell module into a string representation.
showProg :: AHOptions -> Prog a -> String
showProg opts (Prog (mn, _) is tds fds)
  = let opts' = opts {currentModule = mn}
        mn' = if null mn then mn else gsep (unwords ["module", mn, "where"])
        is' = gsep (vsep (map (("import " ++) . fst) is))
        tds' = concatMap (gsep . showTypeDecl opts') tds
        fds' = concatMap (gsep . showFuncDecl opts') fds
     in mn' ++ is' ++ tds' ++ fds'
  where
    gsep :: String -> String
    gsep [] = []
    gsep xs = xs ++ "\n\n"

-- | Transforms an algebraic data type or type synonym declaration into a string
--   representation.
showTypeDecl :: AHOptions -> TypeDecl a -> String
showTypeDecl opts (TypeSyn _ (qn, _) _ vns te)
  = unwords (["type", showQName opts qn] ++ map (showVarName . fst) vns
                                         ++ ["=", showTypeExpr opts te])
showTypeDecl opts (Type _ (qn, _) _ vns cds)
  | null cds  = tdl
  | otherwise
    = vsep ((tdl ++ " = " ++ showConsDecl opts (head cds))
              : map showCD (tail cds))
  where
    tdl = unwords (["data", showQName opts qn] ++ map (showVarName . fst) vns)
    showCD :: ConsDecl a -> String
    showCD cd = indent (length tdl) (" | " ++ showConsDecl opts cd)

-- | Transforms a type constructor declaration into a string representation.
showConsDecl :: AHOptions -> ConsDecl a -> String
showConsDecl opts (Cons _ (qn, _) _ _ tes)
  = unwords (showQName opts qn : map (showTypeExpr' opts 2) tes)

-- | Transforms a type expression into a string representation.
showTypeExpr :: AHOptions -> TypeExpr a -> String
showTypeExpr opts = showTypeExpr' opts 0

-- | Transforms a type expression into a string representation. The integer
--   value is used to add parentheses at the highest level if needed. Possible
--   values are zero (no parentheses), one (parentheses for the left function
--   type expression) and two (parentheses also for type constructors with at
--   least one argument).
showTypeExpr' :: AHOptions -> Int -> TypeExpr a -> String
showTypeExpr' opts = showTypeExpr''
  where
    showTypeExpr'' :: Int -> TypeExpr a -> String
    showTypeExpr'' _ (TVar (v, _))         = showVarName v
    showTypeExpr'' p (FuncType _ t1 t2)
      = parensIf (p > 0) (showTypeExpr'' 1 t1 ++ " -> " ++ showTypeExpr'' 0 t2)
    showTypeExpr'' p (TCons _ (qn, _) tes)
      | snd qn == "[]" && one tes
        = list [showTypeExpr'' 0 (head tes)]
      | isTupleCons qn
        = tuple (map (showTypeExpr'' 0) tes)
      | otherwise
        = parensIf
            (p > 1 && not (null tes))
            (unwords (showQName opts qn : map (showTypeExpr'' 2) tes))

-- | Transforms a type signature for the function with the given qualified name
--   into a string representation.
showTypeSig :: AHOptions -> QName -> TypeSig a -> String
showTypeSig _    _  Untyped      = ""
showTypeSig opts qn (TypeSig te) = showQName opts qn ++ " :: "
                                                     ++ showTypeExpr opts te

-- | Transforms a type annotation into a string representation.
showTypeAnn :: AHOptions -> TypeAnn a -> String
showTypeAnn _    NoTypeAnn    = ""
showTypeAnn opts (TypeAnn te) = showTypeExpr opts te

-- | Transforms a function declaration into a string representation.
showFuncDecl :: AHOptions -> FuncDecl a -> String
showFuncDecl opts (Func _ (qn, _) _ _ ts rs)
  = let ts' = showTypeSig opts qn ts
        rs' = showRules opts qn rs
     in if null ts' then rs' else ts' ++ "\n" ++ rs'

-- | Transforms a rules declaration for the function with the given qualified
--   name into a string representation.
showRules :: AHOptions -> QName -> Rules a -> String
showRules opts qn (Rules rs)     = vsep (map (showRule opts qn) rs)
showRules opts qn (External _ _) = showQName opts qn ++ " external"

-- | Transforms a function rule for the function with the given qualified name
--   into a string representation.
showRule :: AHOptions -> QName -> Rule a -> String
showRule opts qn (Rule _ _ ps rhs _)
  = unwords (showQName opts qn : map (showPattern opts) ps) ++ showRhs opts rhs

-- | Transforms a right-hand side into a string representation.
showRhs :: AHOptions -> Rhs a -> String
showRhs opts (SimpleRhs e)     = " = " ++ showExpr' opts 2 e
showRhs opts (GuardedRhs _ gs) = "\n" ++ vsep (map showGuard gs)
  where
    showGuard :: (Expr a, Expr a) -> String
    showGuard (g, e) = "  | " ++ showExpr' opts 4 g
                              ++ " = "
                              ++ showExpr' opts 4 e

-- | Transforms an expression into a string representation.
showExpr :: AHOptions -> Expr a -> String
showExpr opts = showExpr' opts 0

-- | Transforms an expression into a string representation. The integer value
--   represents the indentation level.
showExpr' :: AHOptions -> Int -> Expr a -> String
showExpr' opts = showExpr'' False
  where
    showExpr'' :: Bool -> Int -> Expr a -> String
    showExpr'' _ _ (Var _ (vn, _))                = showVarName vn
    showExpr'' _ _ (Lit _ (l, _))                 = showLiteral l
    showExpr'' _ _ (Symbol _ (qn, _))             = showQName opts qn
    showExpr'' c n (Apply _ _ e1 e2)
      = parensIf c (showExpr'' False n e1 ++ " " ++ showExpr'' True n e2)
    showExpr'' c n (InfixApply _ _ e1 (qn, _) e2)
      = parensIf c (unwords [showQName opts qn,
                             showExpr'' True n e1,
                             showExpr'' True n e2])
    showExpr'' c n (Lambda _ _ ps e)
      = parensIf True ("\\" ++ unwords (map (showPattern opts) ps)
                            ++ " -> "
                            ++ showExpr'' False n e)
    showExpr'' c n (Case _ _ e bs)
      = parensIf c ("case " ++ showExpr'' False n e
                            ++ " of\n"
                            ++ vsep (map (showBranchExpr opts n) bs))
    showExpr'' c n (Typed _ _ e te)
      = parensIf c (showExpr'' False n e ++ " :: " ++ showTypeExpr opts te)
    showExpr'' c n (IfThenElse _ _ e1 e2 e3)
      = parensIf c (unwords ["if", showExpr'' False n e1,
                             "then", showExpr'' False n e2,
                             "else", showExpr'' False n e3])
    showExpr'' _ n (Tuple _ _ es)
      = tuple (map (showExpr'' False n) es)
    showExpr'' _ n (List _ _ es)
      = list (map (showExpr'' False n) es)
    showExpr'' _ _ _                              = ""

-- | Transforms a pattern into a string representation.
showPattern :: AHOptions -> Pattern a -> String
showPattern opts = showPattern' True
  where
    showPattern' :: Bool -> Pattern a -> String
    showPattern' _ (PVar _ (vn, _))       = showVarName vn
    showPattern' _ (PLit _ (l, _))        = showLiteral l
    showPattern' c (PComb _ _ (qn, _) ps)
      | snd qn == "(:)" && two ps
        = parensIf c (showPattern' True (head ps)
                        ++ (':' : showPattern' True (ps !! 1)))
      | otherwise
        = parensIf c (unwords (showQName opts qn : map (showPattern' True) ps))
    showPattern' _ (PAs _ _ (vn, _) p)
      = showVarName vn ++ ('@' : showPattern' True p)
    showPattern' _ (PTuple _ _ ps)        = tuple (map (showPattern' False) ps)
    showPattern' _ (PList _ _ ps)         = list (map (showPattern' False) ps)

-- | Transforms a branch expression into a string representation.
showBranchExpr :: AHOptions -> Int -> BranchExpr a -> String
showBranchExpr opts n (Branch _ p e)
  = indent n (showPattern opts p ++ " -> " ++ showExpr' opts (n + 2) e)

-- | Transforms a literal into a string representation.
showLiteral :: Literal -> String
showLiteral (Intc i)    = show i
showLiteral (Floatc f)  = show f
showLiteral (Charc c)   = show c
showLiteral (Stringc s) = show s

-- -----------------------------------------------------------------------------
-- Definition of auxiliary functions
-- -----------------------------------------------------------------------------

-- | Checks whether the given qualified name is the tuple type constructor.
isTupleCons :: QName -> Bool
isTupleCons (_, "")   = False
isTupleCons (_, c:cs) = c == '(' && isTupleCons' cs
  where
    isTupleCons' :: String -> Bool
    isTupleCons' ""     = False
    isTupleCons' [x]    = x == ')'
    isTupleCons' (x:xs) = x == ',' && isTupleCons' xs
