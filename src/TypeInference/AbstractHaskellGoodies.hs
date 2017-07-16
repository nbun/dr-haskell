{-|
  This library provides some useful auxiliary functions for abstract Haskell
  data types.
-}

module TypeInference.AbstractHaskellGoodies
  ( preName, tupleName, baseType, boolType, charType, intType, floatType
  , listType, ioType, maybeType, eitherType, stringType, tupleType, literalType
  , typeSigType, typeAnnType, rhsType, exprType, patternType, teVar, (=.=)
  , hasTypeSig
  ) where

import TypeInference.AbstractHaskell

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
boolType x = baseType (preName "Bool") x

-- | Returns the 'Char' type with the given annotation.
charType :: a -> TypeExpr a
charType x = baseType (preName "Char") x

-- | Returns the 'Int' type with the given annotation.
intType :: a -> TypeExpr a
intType x = baseType (preName "Int") x

-- | Returns the 'Float' type with the given annotation.
floatType :: a -> TypeExpr a
floatType x = baseType (preName "Float") x

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

-- | Returns a type variable with the given index and the given annotation.
teVar :: Int -> a -> TypeExpr a
teVar v x = TVar ((v, varToString v), x)

-- | Returns a type expression equation with the given type expressions.
(=.=) :: TypeExpr a -> TypeExpr a -> TypeExprEq a
te1 =.= te2 = (te1, te2)

-- | Checks whether the given function declaration has a type signature.
hasTypeSig :: FuncDecl a -> Bool
hasTypeSig (Func _ _ _ _ Untyped _)     = False
hasTypeSig (Func _ _ _ _ (TypeSig _) _) = True