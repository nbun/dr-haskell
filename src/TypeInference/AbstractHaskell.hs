{-|
  This library contains data types for representing Haskell programs in Haskell.
-}

module TypeInference.AbstractHaskell
  ( MName, QName, VarName, Arity, TypeExprEq, TypeExprEqs, Visibility (..)
  , Prog (..), TypeDecl (..), ConsDecl (..), TypeExpr (..), TypeSig (..)
  , TypeAnn (..), FuncDecl (..), Rules (..), Rule (..), Rhs (..), LocalDecl (..)
  , Expr (..), Statement (..), Pattern (..), BranchExpr (..), Literal (..)
  , AHOptions (..)
  , varToString, defaultAHOptions, showQName, showVarName, showTypeExpr
  , showTypeSig, showTypeAnn, showLiteral
  ) where

import Goodies (one, parensIf, tupled)

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

-- | Transforms a type expression into a string representation.
showTypeExpr :: AHOptions -> TypeExpr a -> String
showTypeExpr opts = showTypeExpr' 0
  where
    showTypeExpr' :: Int -> TypeExpr a -> String
    showTypeExpr' _ (TVar (v, _))         = showVarName v
    showTypeExpr' p (FuncType _ t1 t2)
      = parensIf (p > 0) (showTypeExpr' 1 t1 ++ " -> " ++ showTypeExpr opts t2)
    showTypeExpr' p (TCons _ (qn, _) tes)
      | snd qn == "[]" && one tes
        = '[' : showTypeExpr opts (head tes) ++ "]"
      | isTupleCons qn
        = tupled (map (showTypeExpr opts) tes)
      | otherwise
        = parensIf
            (p > 1 && not (null tes))
            (unwords (showQName opts qn : map (showTypeExpr' 2) tes))

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
