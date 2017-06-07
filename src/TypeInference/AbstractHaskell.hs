------------------------------------------------------------------------------
--- Library to support meta-programming in Curry.
---
--- This library contains a definition for representing Haskell or Curry
--- programs in Curry (type "Prog").
---
--- Note: this definition contains support for type classes which
--- are currently not part of Curry but can be used to generate also
--- Haskell programs. It also contains representation of logic variables
--- which are not part of Haskell.
---
--- @author Michael Hanus, Björn Peemöller
--- @version May 2015
------------------------------------------------------------------------------

module AbstractHaskell where

------------------------------------------------------------------------------
-- Definition of data types for representing abstract Curry programs:
-- ==================================================================

--- Data type for representing a Curry module in the intermediate form.
--- A value of this data type has the form
--- <CODE>
---  (CProg modname imports typedecls functions opdecls)
--- </CODE>
--- where modname: name of this module,
---       imports: list of modules names that are imported,
---       typedecls, opdecls, functions: see below
data Prog a = Prog a String [String] [TypeDecl a] [FuncDecl a] [OpDecl a]

--- The data type for representing qualified names.
--- In AbstractHaskell all names are qualified to avoid name clashes.
--- The first component is the module name and the second component the
--- unqualified name as it occurs in the source program.
type QName a = (a,String, String)

--- Data type to specify the visibility of various entities.
data Visibility = Public  -- exported entity
                  | Private  -- private entity

--- The data type for representing type variables.
--- They are represented by (i,n) where i is a type variable index
--- which is unique inside a function and n is a name (if possible,
--- the name written in the source program).
type TVarIName  a = (a, Int, String)

--- Data type for representing definitions of algebraic data types
--- and type synonyms.
---
--- A data type definition of the form
---
--- <code>data t x1...xn = ...| c t1....tkc |...</code>
---
--- is represented by the Curry term
---
--- <code>(Type t v [i1,...,in] [...(ons c kc v [t1,...,tkc])...])</code>
---
--- where each <code>ij</code> is the index of the type variable
--- <code> xj</code>.
---
--- Note: the type variable indices are unique inside each type declaration
---       and are usually numbered from 0
---
--- Thus, a data type declaration consists of the name of the data type,
--- a list of type parameters and a list of constructor declarations.
data TypeDecl a
  = Type   a   (QName a) Visibility [TVarIName a] [ConsDecl a]
  | TypeSyn  a (QName a) Visibility [TVarIName a] (TypeExpr a)
  | Instance  a (QName a) (TypeExpr a) [Context a] [(QName a, Rule a)]

--- A single type context is class name applied to type variables.
data Context a = Context (QName a) [TVarIName a]

--- A constructor declaration consists of the name and arity of the
--- constructor and a list of the argument types of the constructor.
data ConsDecl a = Cons  a (QName a) Int Visibility  [TypeExpr a]


--- Data type for type expressions.
--- A type expression is either a type variable, a function type,
--- or a type constructor application.
---
--- Note: the names of the predefined type constructors are
---       "Int", "Float", "Bool", "Char", "IO", "Success",
---       "()" (unit type), "(,...,)" (tuple types), "[]" (list type)
data TypeExpr a
  = TVar a (TVarIName a)              -- type variable
  | FuncType a (TypeExpr a) (TypeExpr a)  -- function type t1->t2
  | TCons a (QName a) [TypeExpr a]      -- type constructor application
                                -- (TCons (module,name) arguments)

--- Data type to represent the type signature of a defined function.
--- The type can be missing or a type with an optional context.
data TypeSig a
  = Untyped
  | CType a [Context a] (TypeExpr a)

--- Data type for operator declarations.
--- An operator declaration "fix p n" in Curry corresponds to the
--- AbstractHaskell term (Op n fix p).
data OpDecl a = Op a (QName a) (Fixity a) Int

data Fixity a
  = InfixOp a  -- non-associative infix operator
  | InfixlOp a -- left-associative infix operator
  | InfixrOp a -- right-associative infix operator

--- Data types for representing object variables.
--- Object variables occurring in expressions are represented by (Var i)
--- where i is a variable index.
type VarIName a = (a, Int, String)

--- Data type for representing function declarations.
---
--- A function declaration in AbstractHaskell is a term of the form
---
--- <code>
--- (Func cmt name arity visibility type (Rules eval [Rule rule1,...,rulek]))
--- </code>
---
--- and represents the function <code>name</code> defined by the rules
--- <code>rule1,...,rulek</code>.
---
--- Note: the variable indices are unique inside each rule
---
--- External functions are represented as
--- <code>(Func cmt name arity type External)</code>.
---
--- Thus, a function declaration consists of the comment, name, arity, type,
--- and a list of rules. The type is optional according to its occurrence in
--- the source text. The comment could be used
--- by pretty printers that generate a readable Curry program
--- containing documentation comments.
data FuncDecl a = Func a String (QName a) Int Visibility (TypeSig a) (Rules a)

--- Rules are either a list of single rules or no rule at all
--- if then function is defined externally.
data Rules a
  = Rules a [Rule a]
  | External a

--- The most general form of a rule. It consists of a list of patterns
--- (left-hand side), a list of guards ("success" if not present in the
--- source text) with their corresponding right-hand sides, and
--- a list of local declarations.
data Rule a = Rule a [Pattern a] (Rhs a) [LocalDecl a]

data Rhs a
  = SimpleRhs  a (Expr a)
  | GuardedRhs a [(Statement a, Expr a)]

--- Data type for representing local (let/where) declarations
data LocalDecl a
  = LocalFunc a  (FuncDecl a)                 -- local function declaration
  | LocalPat a (Pattern a) (Rhs a) [LocalDecl a] -- local pattern declaration

--- Data type for representing Haskell expressions.
data Expr a
  = Var      a  (VarIName a)          -- variable (unique index / name)
  | Lit      a  Literal       -- literal (Integer/Float/Char constant)
  | Symbol   a  (QName  a)            -- a defined symbol with module and name
  | Apply    a  (Expr a) (Expr a)         -- application (e1 e2)
  | InfixApply a (Expr a) (QName a) (Expr a)   -- infix application
  | Lambda     a [Pattern a] (Expr a)    -- lambda abstraction
  | Let        a [LocalDecl a] (Expr a)  -- local let declarations
  | DoExpr     a [Statement a]       -- do expression
  | ListComp   a (Expr a) [Statement a]  -- list comprehension
  | Case       a (Expr a) [BranchExpr a]  -- case expression
  | Typed     a (Expr a) (TypeExpr a)     -- typed expression
  | IfThenElse a (Expr a) (Expr a) (Expr a)    -- if-then-else expression
  | Tuple      a [Expr a]
  | List        a [Expr a]

--- Data type for representing statements in do expressions and
--- list comprehensions.
data Statement a
  = SExpr a (Expr a)        -- an expression (I/O action or boolean)
  | SPat a (Pattern a) (Expr a) -- a pattern definition
  | SLet a [LocalDecl a]  -- a local let declaration

--- Data type for representing pattern expressions.
data Pattern a
  = PVar  a (VarIName a)              -- pattern variable (unique index / name)
  | PLit a Literal             -- literal (Integer/Float/Char constant)
  | PComb a (QName a) [Pattern a]      -- application (m.c e1 ... en) of n-ary
                               -- constructor m.c (PComb (m,c) [e1,...,en])
  | PAs a (VarIName a) (Pattern a)       -- as-pattern (extended Curry)
  | PTuple a [Pattern a]
  | PList a [Pattern a]

--- Data type for representing branches in case expressions.
data BranchExpr a = Branch a (Pattern a) (Expr a)

--- Data type for representing literals occurring in an expression.
--- It is either an integer, a float, a character, or a string constant.
data Literal
  = Intc    Int
  | Floatc  Float
  | Charc   Char
  | Stringc String

