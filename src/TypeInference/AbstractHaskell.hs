module TypeInference.AbstractHaskell where

type MName = String

type QName = (MName, String)

type VarName = (Int, String)

type Arity = Int

data Visibility = Private
                | Public

data Prog a = Prog (MName, a) [(MName, a)] [TypeDecl a] [FuncDecl a]

data TypeDecl a = Type a (QName, a) Visibility [(VarName, a)] [ConsDecl a]
                | TypeSyn a (QName, a) Visibility [(VarName, a)] (TypeExpr a)

data ConsDecl a = Cons a (QName, a) Arity Visibility [TypeExpr a]

data TypeInfo a = TExpr (TypeExpr a)
                | TNone

data TypeExpr a = TVar (VarName, a)
                | FuncType a (TypeExpr a) (TypeExpr a)
                | TCons a (QName, a) [TypeExpr a]

data TypeSig a = Untyped
               | CType a (TypeExpr a)

data FuncDecl a = Func a (QName, a) Arity Visibility (TypeSig a) (TypeInfo a) (Rules a)

data Rules a = Rules [Rule a]
             | External a (TypeExpr a)

data Rule a = Rule a (TypeInfo a) [Pattern a] (Rhs a) [LocalDecl a]

data Rhs a = SimpleRhs  a (Expr a)
           | GuardedRhs a [(Statement a, Expr a)]

data LocalDecl a = LocalFunc a (FuncDecl a)
                 | LocalPat a (Pattern a) (Rhs a) [LocalDecl a]

data Expr a = Var (TypeInfo a) (VarName, a)
            | Lit (TypeInfo a) (Literal, a)
            | Symbol (TypeInfo a) (QName, a)
            | Apply a (TypeInfo a) (Expr a) (Expr a)
            | InfixApply a (TypeInfo a) (Expr a) (QName, a) (Expr a)
            | Lambda a (TypeInfo a) [Pattern a] (Expr a)
            | Let a (TypeInfo a) [LocalDecl a] (Expr a)
            | DoExpr a (TypeInfo a) [Statement a]
            | ListComp a (TypeInfo a) (Expr a) [Statement a]
            | Case a (TypeInfo a) (Expr a) [BranchExpr a]
            | Typed a (Expr a) (TypeExpr a)
            | IfThenElse a (TypeInfo a) (Expr a) (Expr a) (Expr a)
            | Tuple a (TypeInfo a) [Expr a]
            | List a (TypeInfo a) [Expr a]

data Statement a = SExpr a (TypeInfo a) (Expr a)
                 | SPat a (TypeInfo a) (Pattern a) (Expr a)
                 | SLet a (TypeInfo a) [LocalDecl a]

data Pattern a = PVar (TypeInfo a) (VarName, a)
               | PLit (TypeInfo a) (Literal, a)
               | PComb a (TypeInfo a) (QName, a) [Pattern a]
               | PAs a (TypeInfo a) (VarName, a) (Pattern a)
               | PTuple a (TypeInfo a) [Pattern a]
               | PList a (TypeInfo a) [Pattern a]

data BranchExpr a = Branch a (Pattern a) (Expr a)

data Literal = Intc Int
             | Floatc Float
             | Charc Char
             | Stringc String