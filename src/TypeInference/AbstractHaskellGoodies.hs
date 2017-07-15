{-|
  This library provides some useful auxiliary functions for abstract Haskell
  data types.
-}

module TypeInference.AbstractHaskellGoodies
  ( preName, tupleName, baseType, boolType, charType, intType, floatType
  , listType, ioType, maybeType, eitherType, stringType, tupleType, literalType
  , teVar, (=.=), hasTypeSig
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