module TypeInference.AbstractHaskell where

type MName = String

type QName = (MName, String)

type VarName = (Int, String)

type Arity = Int

data Visibility = Private | Public

data Prog a = Prog (MName, a) [(MName, a)] [TypeDecl a] [FuncDecl a]

data TypeDecl a = Type a (QName, a) Visibility [(VarName, a)] [ConsDecl a]
                | TypeSyn a (QName, a) Visibility [(VarName, a)] (TypeExpr a)

data ConsDecl a = Cons a (QName, a) Arity Visibility [TypeExpr a]

data TypeExpr a = TVar (VarName, a)
                | FuncType a (TypeExpr a) (TypeExpr a)
                | TCons a (QName, a) [TypeExpr a]

data TypeSig a = Untyped
               | TypeSig (TypeExpr a)

data TypeAnn a = TypeAnn (TypeExpr a)
               | NoAnn

data FuncDecl a
  = Func a (QName, a) Arity Visibility (TypeSig a) (Rules a)

data Rules a = Rules [Rule a]
             | External a (TypeAnn a)

data Rule a = Rule a (TypeAnn a) [Pattern a] (Rhs a) [LocalDecl a]

data Rhs a = SimpleRhs (Expr a)
           | GuardedRhs a [(Statement a, Expr a)]

data LocalDecl a = LocalFunc (FuncDecl a)
                 | LocalPat a (Pattern a) (Expr a) [LocalDecl a]

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
            | Typed a (Expr a) (TypeExpr a)
            | IfThenElse a (TypeAnn a) (Expr a) (Expr a) (Expr a)
            | Tuple a (TypeAnn a) [Expr a]
            | List a (TypeAnn a) [Expr a]

data Statement a = SExpr (Expr a)
                 | SPat a (Pattern a) (Expr a)
                 | SLet a [LocalDecl a]

data Pattern a = PVar (TypeAnn a) (VarName, a)
               | PLit (TypeAnn a) (Literal, a)
               | PComb a (TypeAnn a) (QName, a) [Pattern a]
               | PAs a (TypeAnn a) (VarName, a) (Pattern a)
               | PTuple a (TypeAnn a) [Pattern a]
               | PList a (TypeAnn a) [Pattern a]

data BranchExpr a = Branch a (Pattern a) (Expr a)

data Literal = Intc Int
             | Floatc Float
             | Charc Char
             | Stringc String