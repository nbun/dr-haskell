{-|
  This library provides some useful auxiliary functions for abstract Haskell
  data types.
-}

module TypeInference.AbstractHaskellGoodies
  ( preName, tupleName, baseType, boolType, charType, intType, floatType
  , listType, ioType, maybeType, eitherType, stringType, tupleType, literalType
  , typeSigType, typeAnnType, rhsType, exprType, patternType, exprAnn, teVar
  , (=.=), hasTypeSig, funcName, depGraph
  ) where

import TypeInference.AbstractHaskell
import TypeInference.SCC (scc)

-- -----------------------------------------------------------------------------
-- Definition of auxiliary functions for abstract Haskell data types
-- -----------------------------------------------------------------------------

-- | Converts a string into a qualified name of the 'Prelude'.
preName :: String -> QName
preName n = ("Prelude", n)

-- | Returns the qualified name of the tuple type constructor with the given
--   number of components.
tupleName :: Int -> QName
tupleName n | n == 0    = preName "()"
            | n > 1     = preName ("(" ++ replicate (n - 1) ',' ++ ")")
            | otherwise = error err
  where
    err = "The arity of a tuple type constructor can not be one or negative!"

-- | Returns a base type (type constructor without arguments) with the given
--   qualified name and the given annotation.
baseType :: QName -> a -> TypeExpr a
baseType qn x = TCons x (qn, x) []

-- | Returns the 'Bool' type with the given annotation.
boolType :: a -> TypeExpr a
boolType = baseType (preName "Bool")

-- | Returns the 'Char' type with the given annotation.
charType :: a -> TypeExpr a
charType = baseType (preName "Char")

-- | Returns the 'Int' type with the given annotation.
intType :: a -> TypeExpr a
intType = baseType (preName "Int")

-- | Returns the 'Float' type with the given annotation.
floatType :: a -> TypeExpr a
floatType = baseType (preName "Float")

-- | Returns a list type with the given type expression and the given
--   annotation.
listType :: TypeExpr a -> a -> TypeExpr a
listType te x = TCons x (preName "[]", x) [te]

-- | Returns the 'IO' type with the given type expression and the given
--   annotation.
ioType :: TypeExpr a -> a -> TypeExpr a
ioType te x = TCons x (preName "IO", x) [te]

-- | Returns the 'Maybe' type with the given type expression and the given
--   annotation.
maybeType :: TypeExpr a -> a -> TypeExpr a
maybeType te x = TCons x (preName "Maybe", x) [te]

-- | Returns the 'Either' type with the given type expressions and the given
--   annotation.
eitherType :: TypeExpr a -> TypeExpr a -> a -> TypeExpr a
eitherType te1 te2 x = TCons x (preName "Either", x) [te1, te2]

-- | Returns the 'String' type with the given annotation.
stringType :: a -> TypeExpr a
stringType x = listType (charType x) x

-- | Returns a tuple type with the given list of type expressions and the given
--   annotation.
tupleType :: [TypeExpr a] -> a -> TypeExpr a
tupleType [te] _ = te
tupleType tes  x = TCons x (tupleName (length tes), x) tes

-- | Converts the given literal and the given annotation to a literal type.
literalType :: Literal -> a -> TypeExpr a
literalType (Intc _)    = intType
literalType (Floatc _)  = floatType
literalType (Charc _)   = charType
literalType (Stringc _) = stringType

-- | Returns the type expression from the given type signature or 'Nothing' if
--   no such type expression exists.
typeSigType :: TypeSig a -> Maybe (TypeExpr a)
typeSigType Untyped      = Nothing
typeSigType (TypeSig te) = Just te

-- | Returns the type expression from the given type annotation or 'Nothing' if
--   no type is annotated.
typeAnnType :: TypeAnn a -> Maybe (TypeExpr a)
typeAnnType NoTypeAnn    = Nothing
typeAnnType (TypeAnn te) = Just te

-- | Returns the list of type expressions from the given right-hand side.
rhsType :: Rhs a -> [Maybe (TypeExpr a)]
rhsType (SimpleRhs e)      = [exprType e]
rhsType (GuardedRhs _ eqs) = map (exprType . snd) eqs

-- | Returns the annotated type from the given expression or 'Nothing' if no
--   type is annotated.
exprType :: Expr a -> Maybe (TypeExpr a)
exprType (Var ta _)              = typeAnnType ta
exprType (Lit ta _)              = typeAnnType ta
exprType (Symbol ta _)           = typeAnnType ta
exprType (Apply _ ta _ _)        = typeAnnType ta
exprType (InfixApply _ ta _ _ _) = typeAnnType ta
exprType (Lambda _ ta _ _)       = typeAnnType ta
exprType (Let _ ta _ _)          = typeAnnType ta
exprType (DoExpr _ ta _)         = typeAnnType ta
exprType (ListComp _ ta _ _)     = typeAnnType ta
exprType (Case _ ta _ _)         = typeAnnType ta
exprType (Typed _ ta _ _)        = typeAnnType ta
exprType (IfThenElse _ ta _ _ _) = typeAnnType ta
exprType (Tuple _ ta _)          = typeAnnType ta
exprType (List _ ta _)           = typeAnnType ta

-- | Returns the annotated type from the given pattern or 'Nothing' if no type
--   is annotated.
patternType :: Pattern a -> Maybe (TypeExpr a)
patternType (PVar ta _)      = typeAnnType ta
patternType (PLit ta _)      = typeAnnType ta
patternType (PComb _ ta _ _) = typeAnnType ta
patternType (PAs _ ta _ _)   = typeAnnType ta
patternType (PTuple _ ta _)  = typeAnnType ta
patternType (PList _ ta _)   = typeAnnType ta

-- | Returns the annotation from the given expression.
exprAnn :: Expr a -> a
exprAnn (Var _ (_, x))         = x
exprAnn (Lit _ (_, x))         = x
exprAnn (Symbol _ (_, x))      = x
exprAnn (Apply x _ _ _)        = x
exprAnn (InfixApply x _ _ _ _) = x
exprAnn (Lambda x _ _ _)       = x
exprAnn (Let x _ _ _)          = x
exprAnn (DoExpr x _ _)         = x
exprAnn (ListComp x _ _ _)     = x
exprAnn (Case x _ _ _)         = x
exprAnn (Typed x _ _ _)        = x
exprAnn (IfThenElse x _ _ _ _) = x
exprAnn (Tuple x _ _)          = x
exprAnn (List x _ _)           = x

-- | Returns a type variable with the given index and the given annotation.
teVar :: Int -> a -> TypeExpr a
teVar v x = TVar ((v, varToString v), x)

-- | Returns a type expression equation with the given type expressions.
(=.=) :: TypeExpr a -> TypeExpr a -> TypeExprEq a
(=.=) = (,)

-- | Checks whether the given function declaration has a type signature.
hasTypeSig :: FuncDecl a -> Bool
hasTypeSig (Func _ _ _ _ Untyped _)     = False
hasTypeSig (Func _ _ _ _ (TypeSig _) _) = True

-- | Returns the qualified name of the given function declaration.
funcName :: FuncDecl a -> QName
funcName (Func _ (qn, _) _ _ _ _) = qn

-- -----------------------------------------------------------------------------
-- Functions for computation of function dependency graphs
-- -----------------------------------------------------------------------------

-- | Returns the strongly connected components of the given list of function
--   declarations within the module with the given name.
depGraph :: MName -> [FuncDecl a] -> [[FuncDecl a]]
depGraph mn = scc (pure . funcName) use
  where
    use :: FuncDecl a -> [QName]
    use (Func _ _ _ _ _ rs) = calledRS rs

    calledRS :: Rules a -> [QName]
    calledRS (Rules rs)     = concatMap calledR rs
    calledRS (External _ _) = []

    calledR :: Rule a -> [QName]
    calledR (Rule _ _ _ rhs lds) = calledRhs rhs ++ concatMap calledLD lds

    calledRhs :: Rhs a -> [QName]
    calledRhs (SimpleRhs e)      = called e
    calledRhs (GuardedRhs _ eqs)
      = concatMap (\(l, r) -> called l ++ called r) eqs

    calledLD :: LocalDecl a -> [QName]
    calledLD (LocalFunc fd)       = use fd
    calledLD (LocalPat _ _ e lds) = called e ++ concatMap calledLD lds

    called :: Expr a -> [QName]
    called (Var _ _)                         = []
    called (Lit _ _)                         = []
    called (Symbol _ (qn, _)) | fst qn == mn = [qn]
                              | otherwise    = []
    called (Apply _ _ e1 e2)                 = called e1 ++ called e2
    called (InfixApply _ _ e1 (qn, _) e2)
      | fst qn == mn                         = [qn] ++ called e1 ++ called e2
      | otherwise                            = called e1 ++ called e2
    called (Lambda _ _ _ e)                  = called e
    called (Let _ _ lds e)
      = concatMap calledLD lds ++ called e
    called (DoExpr _ _ sts)                  = concatMap calledS sts
    called (ListComp _ _ e sts)              = called e ++ concatMap calledS sts
    called (Case _ _ e bs)                   = called e ++ concatMap calledBE bs
    called (Typed _ _ e _)                   = called e
    called (IfThenElse _ _ e1 e2 e3)         = concatMap called [e1, e2, e3]
    called (Tuple _ _ es)                    = concatMap called es
    called (List _ _ es)                     = concatMap called es

    calledS :: Statement a -> [QName]
    calledS (SExpr e)    = called e
    calledS (SPat _ _ e) = called e
    calledS (SLet _ lds) = concatMap calledLD lds

    calledBE :: BranchExpr a -> [QName]
    calledBE (Branch _ _ e) = called e