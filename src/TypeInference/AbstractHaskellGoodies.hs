{-|
  This library provides some useful auxiliary functions for abstract Haskell
  data types.
-}

module TypeInference.AbstractHaskellGoodies
  ( pre, preName, tupleName, baseType, boolType, charType, stringType, intType
  , floatType, orderingType, listType, ioType, maybeType, eitherType, tupleType
  , literalType, typeSigType, typeAnnType, rulesTypes, ruleType, rhsTypes
  , exprType, patternType, typeExprAnn, exprAnn, teVar, (=.=), hasTypeSig
  , funcName, modName, leftFuncType, rightFuncType, returnType, depGraph
  ) where

import SCC                           (scc)
import TypeInference.AbstractHaskell

-- -----------------------------------------------------------------------------
-- Definition of auxiliary functions for abstract Haskell data types
-- -----------------------------------------------------------------------------

-- | The module name of the 'Prelude'.
pre :: MName
pre = "Prelude"

-- | Converts a string into a qualified name of the 'Prelude'.
preName :: String -> QName
preName n = (pre, n)

-- | Returns the qualified name of the tuple type constructor with the given
--   number of components.
tupleName :: Int -> QName
tupleName n | n == 0    = preName "()"
            | n > 1     = preName ('(' : replicate (n - 1) ',' ++ ")")
            | otherwise = error err
  where
    err = "The arity of a tuple type constructor can not be one or negative!"

-- | Returns a base type (type constructor without arguments) with the given
--   qualified name and the given annotations.
baseType :: QName -> a -> a -> TypeExpr a
baseType qn x y = TCons x (qn, y) []

-- | Returns the 'Bool' type with the given annotations.
boolType :: a -> a -> TypeExpr a
boolType = baseType (preName "Bool")

-- | Returns the 'Char' type with the given annotations.
charType :: a -> a -> TypeExpr a
charType = baseType (preName "Char")

-- | Returns the 'String' type with the given annotations.
stringType :: a -> a -> TypeExpr a
stringType = baseType (preName "String")

-- | Returns the 'Int' type with the given annotations.
intType :: a -> a -> TypeExpr a
intType = baseType (preName "Int")

-- | Returns the 'Float' type with the given annotations.
floatType :: a -> a -> TypeExpr a
floatType = baseType (preName "Float")

-- | Returns the 'Ordering' type with the given annotations.
orderingType :: a -> a -> TypeExpr a
orderingType = baseType (preName "Ordering")

-- | Returns a list type with the given type expression and the given
--   annotations.
listType :: TypeExpr a -> a -> a -> TypeExpr a
listType te x y = TCons x (preName "[]", y) [te]

-- | Returns the 'IO' type with the given type expression and the given
--   annotations.
ioType :: TypeExpr a -> a -> a -> TypeExpr a
ioType te x y = TCons x (preName "IO", y) [te]

-- | Returns the 'Maybe' type with the given type expression and the given
--   annotations.
maybeType :: TypeExpr a -> a -> a -> TypeExpr a
maybeType te x y = TCons x (preName "Maybe", y) [te]

-- | Returns the 'Either' type with the given type expressions and the given
--   annotations.
eitherType :: TypeExpr a -> TypeExpr a -> a -> a -> TypeExpr a
eitherType te1 te2 x y = TCons x (preName "Either", y) [te1, te2]

-- | Returns a tuple type with the given list of type expressions and the given
--   annotations.
tupleType :: [TypeExpr a] -> a -> a -> TypeExpr a
tupleType [te] _ _ = te
tupleType tes  x y = TCons x (tupleName (length tes), y) tes

-- | Converts the given literal and the given annotations to a literal type.
literalType :: Literal -> a -> a -> TypeExpr a
literalType (Intc _)    x y = intType x y
literalType (Floatc _)  x y = floatType x y
literalType (Charc _)   x y = charType x y
literalType (Stringc _) x y = listType (charType x y) x y

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

-- | Returns the list of annotated types from the given rules declaration.
rulesTypes :: Rules a -> [Maybe (TypeExpr a)]
rulesTypes (Rules rs)      = map ruleType rs
rulesTypes (External _ ta) = [typeAnnType ta]

-- | Returns the annotated type from the given function rule or 'Nothing' if no
--   type is annotated.
ruleType :: Rule a -> Maybe (TypeExpr a)
ruleType (Rule _ ta _ _ _) = typeAnnType ta

-- | Returns the list of type expressions from the given right-hand side.
rhsTypes :: Rhs a -> [Maybe (TypeExpr a)]
rhsTypes (SimpleRhs e)      = [exprType e]
rhsTypes (GuardedRhs _ eqs) = map (exprType . snd) eqs

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

-- | Returns the annotation from the given type expression.
typeExprAnn :: TypeExpr a -> a
typeExprAnn (TVar (_, x))    = x
typeExprAnn (FuncType x _ _) = x
typeExprAnn (TCons x _ _)    = x

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

-- | Returns the module name of the given program.
modName :: Prog a -> MName
modName (Prog (mn, _) _ _ _) = mn

-- | Returns the left type expression from the given function type expression.
leftFuncType :: TypeExpr a -> TypeExpr a
leftFuncType (FuncType _ te _) = te
leftFuncType _
  = error "The given type expression is not a function type!"

-- | Returns the right type expression from the given function type expression.
rightFuncType :: TypeExpr a -> TypeExpr a
rightFuncType (FuncType _ _ te) = te
rightFuncType _
  = error "The given type expression is not a function type!"

-- | Returns the return type expression of the given function type expression.
returnType :: TypeExpr a -> TypeExpr a
returnType (FuncType _ _ te) = returnType te
returnType te                = te

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
    calledRS (Rules rs) = concatMap calledR rs
    calledRS _          = []

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
