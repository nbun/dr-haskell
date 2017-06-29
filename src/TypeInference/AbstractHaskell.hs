module TypeInference.AbstractHaskell
  ( MName, QName, VarName, Arity, Visibility (..), Prog (..), TypeDecl (..)
  , ConsDecl (..), TypeExpr (..), TypeSig (..), TypeAnn (..), FuncDecl (..)
  , Rules (..), Rule (..), Rhs (..), LocalDecl (..), Expr (..), Statement (..)
  , Pattern (..), BranchExpr (..), Literal (..), AHOptions (..)
  , defaultAHOptions, showQName, showVarName, showTypeExpr
  ) where

import Data.List (intercalate)

-- -----------------------------------------------------------------------------
-- Representation of Haskell source code
-- -----------------------------------------------------------------------------

-- A module name represented as a string.
type MName = String

-- A qualified name represented as a tuple consisting of a module name and a
-- string.
type QName = (MName, String)

-- A variable name represented as a tuple consisting of a variable index and a
-- corresponding string representation. The variable index is represented by an
-- integer greater than or equal to zero.
type VarName = (Int, String)

-- The arity of a function or type constructor represented as an integer greater
-- than or equal to zero.
type Arity = Int

-- The visibility of a function, type constructor or type is either private (not
-- exported) or public (exported).
data Visibility = Private | Public
  deriving Show

-- Representation of a Haskell module consisting of a module name, a list of
-- imported modules, a list of type declarations and a list of function
-- declarations.
data Prog a = Prog (MName, a) [(MName, a)] [TypeDecl a] [FuncDecl a]
  deriving Show

-- Representation of an algebraic data type or type synonym declaration.
data TypeDecl a = Type a (QName, a) Visibility [(VarName, a)] [ConsDecl a]
                | TypeSyn a (QName, a) Visibility [(VarName, a)] (TypeExpr a)
  deriving Show

-- Representation of a type constructor declaration consisting of a type
-- constructor name, the arity and visibility of the type constructor and a list
-- of argument types.
data ConsDecl a = Cons a (QName, a) Arity Visibility [TypeExpr a]
  deriving Show

-- Representation of a type expression. A type expression is either a type
-- variable, a function type or a type constructor application.
data TypeExpr a = TVar (VarName, a)
                | FuncType a (TypeExpr a) (TypeExpr a)
                | TCons a (QName, a) [TypeExpr a]
  deriving Show

-- Representation of a type signature for a function.
data TypeSig a = Untyped
               | TypeSig (TypeExpr a)
  deriving Show

-- Representation of a type annotation used to annotate the type of expressions.
data TypeAnn a = NoTypeAnn
               | TypeAnn (TypeExpr a)
  deriving Show

-- Representation of a function declaration consisting of a function name, the
-- arity and visibility of the function, a type signature and a list of rules.
data FuncDecl a = Func a (QName, a) Arity Visibility (TypeSig a) (Rules a)
  deriving Show

-- Rules are either a list of single rules or no rule at all if the function is
-- defined externally.
data Rules a = Rules [Rule a]
             | External a (TypeAnn a)
  deriving Show

-- Representation of a function rule consisting of a type annotation, a list of
-- patterns, a right-hand side and a list of local declarations.
data Rule a = Rule a (TypeAnn a) [Pattern a] (Rhs a) [LocalDecl a]
  deriving Show

-- Representation of a rules right-hand side as either guarded or unguarded.
data Rhs a = SimpleRhs (Expr a)
           | GuardedRhs a [(Expr a, Expr a)]
  deriving Show

-- Representation of local 'let' or 'where' declarations.
data LocalDecl a = LocalFunc (FuncDecl a)
                 | LocalPat a (Pattern a) (Expr a) [LocalDecl a]
  deriving Show

-- Representation of expressions.
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

-- Representation of statements in 'do' expressions or list comprehensions.
data Statement a = SExpr (Expr a)
                 | SPat a (Pattern a) (Expr a)
                 | SLet a [LocalDecl a]
  deriving Show

-- Representation of pattern expressions.
data Pattern a = PVar (TypeAnn a) (VarName, a)
               | PLit (TypeAnn a) (Literal, a)
               | PComb a (TypeAnn a) (QName, a) [Pattern a]
               | PAs a (TypeAnn a) (VarName, a) (Pattern a)
               | PTuple a (TypeAnn a) [Pattern a]
               | PList a (TypeAnn a) [Pattern a]
  deriving Show

-- Representation of branches in 'case' expressions.
data BranchExpr a = Branch a (Pattern a) (Expr a)
  deriving Show

-- Representation of literals occurring in expressions. It is either an integer,
-- a float, a character, or a string constant.
data Literal = Intc Int
             | Floatc Float
             | Charc Char
             | Stringc String
  deriving Show

-- -----------------------------------------------------------------------------
-- Pretty-printing of abstract Haskell data types
-- -----------------------------------------------------------------------------

-- Representation of pretty-printing options for abstract Haskell data types.
data AHOptions = AHOptions { currentModule :: String, unqModules :: [String] }

-- The default pretty-printing options.
defaultAHOptions :: AHOptions
defaultAHOptions = AHOptions { currentModule = "", unqModules = [] }

-- Transforms a qualified name into a string representation.
showQName :: AHOptions -> QName -> String
showQName opts (mn, n) | mn == (currentModule opts) = n
                       | elem mn (unqModules opts)  = n
                       | otherwise                  = mn ++ "." ++ n

-- Transforms a variable name into a string representation.
showVarName :: VarName -> String
showVarName = snd

-- Transforms a type expression into a string representation.
showTypeExpr :: AHOptions -> TypeExpr a -> String
showTypeExpr opts = showTypeExpr' 0
  where
    showTypeExpr' :: Int -> TypeExpr a -> String
    showTypeExpr' _ (TVar (v, _))         = showVarName v
    showTypeExpr' p (FuncType _ t1 t2)
      = parensIf (p > 0)
                 ((showTypeExpr' 1 t1) ++ " -> " ++ (showTypeExpr opts t2))
    showTypeExpr' p (TCons _ (qn, _) tes)
      | (isList qn) && ((length tes) == 1)
        = "[" ++ (showTypeExpr opts (head tes)) ++ "]"
      | isTuple qn
        = tupled (map (showTypeExpr opts) tes)
      | otherwise
        = parensIf ((p > 1) && (not (null tes)))
                   (wsep ((showQName opts qn):(map (showTypeExpr' 2) tes)))
    wsep :: [String] -> String
    wsep = intercalate " "

-- -----------------------------------------------------------------------------
-- Definition of helper functions
-- -----------------------------------------------------------------------------

-- Encloses a string in parenthesis if the given condition is true.
parensIf :: Bool -> String -> String
parensIf b s = if b then "(" ++ s ++ ")" else s

-- Checks whether the given qualified name is the list type constructor.
isList :: QName -> Bool
isList (_, n) = n == "[]"

-- Checks whether the given qualified name is the tuple type constructor.
isTuple :: QName -> Bool
isTuple (_, "")     = False
isTuple (_, (c:cs)) = (c == '(') && (isTuple' cs)
  where
    isTuple' :: String -> Bool
    isTuple' ""           = False
    isTuple' [x]          = x == ')'
    isTuple' (x:xs@(_:_)) = (x == ',') && (isTuple' xs)

-- Returns a string representation of a tuple with the given list of components.
tupled :: [String] -> String
tupled []     = "()"
tupled (x:xs) = "(" ++ x ++ (concatMap (", " ++) xs) ++ ")"
