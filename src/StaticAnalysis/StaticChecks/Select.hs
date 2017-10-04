-- | Utility functions to work with names, modules, etc.
module StaticAnalysis.StaticChecks.Select
  (declName, declsOfModule, defFuncs, defNames, expsOfDecl, getNameOfQName
  , modName, namePos, nameOfModule, nameString, typeSigs, qNameName, qNamesOfExps
  , similar3, varsOfDecl, infixQNames, importedModules
  ) where

import AstChecks.Check                      (mapOverDecls, mapOverExp,
                                             mapOverExpRec)
import Data.Char                            (ord)
import Data.List                            (sortBy)
import Data.Maybe                           (catMaybes, mapMaybe)
import Language.Haskell.Exts
import StaticAnalysis.Messages.StaticErrors (Entity (..))
import Text.EditDistance                    (defaultEditCosts,
                                             levenshteinDistance)

--------------------------------------------------------------------------------
-- Find and modify (qualified) names

-- | Returns names of defined functions of a module
defFuncs :: Module l -> [(Name l, Entity)]
defFuncs m@Module{} = concatMap declName $ funBinds m ++ patBinds m
defFuncs _          = []

-- | Returns names of definitions of a module
defNames :: Module l -> [(Name l, Entity)]
defNames m@Module{} = concatMap declName $
  funBinds m ++ patBinds m ++ dataDecls m
defNames _          = []

-- | Returns name and type of a declaration
declName :: Decl l -> [(Name l, Entity)]
declName d = case d of
               TypeSig _ ns _                 -> map (\n -> (n, Signature)) ns
               FunBind _ (Match _ n _ _ _ :_) -> [(n, Function)]
               FunBind _ (InfixMatch _ _ n _ _ _:_) -> [(n, Function)]
               -- functions without arguments
               PatBind _ (PVar _ n) _ _       -> [(n, Definition)]
               DataDecl _ _ _ (DHead _ n) _ _ -> [(n, Datatype)]
               DataDecl _ _ _ (DHApp _ (DHead _ n) _) _ _ -> [(n, Datatype)]
               InfixDecl _ _ _ ops            -> map opName ops
                 where opName (VarOp _ name) = (name, Function)
                       opName (ConOp _ name) = (name, Function)
               _                              -> []

-- | Returns name of a QName
qNameName :: QName l -> Name l
qNameName (Qual _ (ModuleName _ _) n) = n
qNameName (UnQual _ n) = n
qNameName (Special l specialcon) = Symbol l n
  where n = case specialcon of
              UnitCon _          -> "()"
              ListCon _          -> "[]"
              FunCon  _          -> "->"
              Cons    _          -> "(:)"
              TupleCon _ _ n'    -> '(' : replicate n' ',' ++ ")"
              UnboxedSingleCon _ -> "(# #)"

-- | Returns name of a module if defined
nameOfModule :: Module l -> Maybe (ModuleName l)
nameOfModule (Module _ mhead _ _ _) =
  case mhead of
    Just (ModuleHead _ mname _ _) -> Just mname
    Nothing                       -> Nothing
nameOfModule _ = Nothing

-- | Returns string of a module name
modName :: ModuleName l -> String
modName (ModuleName _ n) = n

-- | Returns string of a name
nameString :: Name l -> String
nameString (Ident  _ s) = s
nameString (Symbol _ s) = s

-- | Returns position of a name
namePos :: Name l -> l
namePos (Ident l _)   = l
namePos (Symbol l _ ) = l

-- | Returns list of imported modules of a module
importedModules :: Module l -> [ModuleName l]
importedModules (Module _ _ _ imps _) = map impName imps
  where impName (ImportDecl _ mname _ _ _ _ _ _) = mname
importedModules _ = []

-- Returns name of a QName as a string
getNameOfQName :: QName l -> String
getNameOfQName (Qual _ _ name) = nameString name
getNameOfQName (UnQual _ name) = nameString name


--------------------------------------------------------------------------------
-- Filter declarations

-- | Returns declarations of a module that fulfill the given predicate
declFilter :: (Decl l -> Bool) -> Module l -> [Decl l]
declFilter p (Module _ _ _ _ decls) = filter p decls
declFilter _ _                      = []

-- | Returns type signatures of a module
typeSigs :: Module l -> [Decl l]
typeSigs = declFilter isTypeSig
  where
    isTypeSig TypeSig{} = True
    isTypeSig _         = False

-- | Returns pattern bindings of a module
patBinds :: Module l -> [Decl l]
patBinds = declFilter isPatBind
  where
    isPatBind PatBind{} = True
    isPatBind _         = False

-- | Returns function bindings of a module
funBinds :: Module l -> [Decl l]
funBinds = declFilter isFunBind
  where
    isFunBind FunBind{} = True
    isFunBind _         = False

-- | Returns infix operators of a module
infixDecls :: Module l -> [Decl l]
infixDecls = declFilter isInfixDecl
  where
    isInfixDecl InfixDecl{} = True
    isInfixDecl _           = False

-- | Returns data declarations of a module
dataDecls :: Module l -> [Decl l]
dataDecls = declFilter isFunBind
  where
    isFunBind :: Decl l -> Bool
    isFunBind DataDecl{} = True
    isFunBind _          = False

--------------------------------------------------------------------------------
-- Find similar names

-- | Calculates the levenshtein distance of two names
calcLev :: Name l -> Name l -> Int
calcLev n m = levenshteinDistance defaultEditCosts s t + penalty
  where s = nameString n
        t = nameString m
        isChar c = let i = ord c in (i > 64 && i < 91) || (i > 96 && i < 123)
        isOp     = foldr (\c b -> isChar c && b) True
        penalty  = if (isOp s && not (isOp t)) || (not (isOp s) && isOp t)
                  then 42 -- Operators like (+) should not be suggested when
                          -- compared to a variable like 'x'
                  else 0

-- | Returns a list of similar names and their levenshtein distances for
-- a given {module, declaration, ...} and a corresponding function that returns
-- the possible names
similar :: a -> (a -> [Name l]) -> Name l -> [(Name l, Int)]
similar m search n = map (\s -> (s, calcLev s n)) (search m)

-- | Works like similar but sorts the result and returns only the top 3 names
-- that differ at at most half of the characters
similar3 :: a -> (a -> [Name l]) -> Name l -> [Name l]
similar3 m search n = take 3 $ map fst sims'
  where sims  = sortBy (\x y -> compare (snd x) (snd y)) (similar m search n)
        sims' = filter  (\x -> snd x <= nlen) sims
        nlen  = (length (nameString n) `div` 2) + 1

--------------------------------------------------------------------------------
-- Find expressions

-- | Returns expressions of a module
expsOfModule :: Module l -> [Exp l]
expsOfModule (Module _ _ _ _ decls) = mapOverDecls (: []) decls
expsOfModule _                      = []

-- | Returns declarations of a module
declsOfModule :: Module l -> [Decl l]
declsOfModule (Module _ _ _ _ decls) = decls
declsOfModule _                      = []

-- | Returns expressions of a declaration
expsOfDecl :: Decl l -> [Exp l]
expsOfDecl d = mapOverDecls (: []) [d]

-- | Returns qualified names of a list of expressions
qNamesOfExps :: [Exp l] -> [QName l]
qNamesOfExps exps = catMaybes $ concatMap (mapOverExp expQName) exps

-- | Returns qualified names of infix operators of a list of expressions
infixQNames :: [Exp l] -> [QName l]
infixQNames = mapMaybe infQn
  where infQn (InfixApp _ _ qop _) = Just $ qOpQn qop
        infQn _                    = Nothing

        qOpQn (QVarOp _ qn) = qn
        qOpQn (QConOp _ qn) = qn

-- | Returns qualified names of an expression
expQName :: Exp l -> [Maybe (QName l)]
expQName (Var _ qn) = [Just qn]
expQName _          = [Nothing]

--------------------------------------------------------------------------------
-- Find variables

-- | Returns variable names of a module
varsOfModule :: Module l -> [Name l]
varsOfModule (Module _ _ _ _ decls) = concatMap varsOfDecl decls
varsOfModule _                      = []

-- | Returns variable names of a Maybe bind
varsOfMaybeBind :: Maybe (Binds l) -> [Name l]
varsOfMaybeBind (Just bind) = varsOfBind bind
varsOfMaybeBind Nothing     = []

-- | Returns variable names of a declaration
varsOfDecl :: Decl l -> [Name l]
varsOfDecl (FunBind _ matches)       = concatMap varsOfMatch matches
varsOfDecl (PatBind _ pat rhs mbind) =
  varsOfPat pat ++ varsOfRhs rhs ++ varsOfMaybeBind mbind
varsOfDecl _                         = []

-- | Returns variable names of a match
varsOfMatch :: Match l -> [Name l]
varsOfMatch (Match _ name pats rhs mbind) = [name] ++
  concatMap varsOfPat pats ++ varsOfMaybeBind mbind ++ varsOfRhs rhs
varsOfMatch (InfixMatch _ pat _ pats rhs mbind) =
  concatMap varsOfPat (pat:pats) ++ varsOfMaybeBind mbind ++ varsOfRhs rhs

-- | Returns variable names of a right-hand side
varsOfRhs :: Rhs l -> [Name l]
varsOfRhs (UnGuardedRhs _ e)   = varsOfExp e
varsOfRhs (GuardedRhss _ grhs) = concatMap varsOfGRhs grhs

-- | Returns variable names of a guarded right-hand side
varsOfGRhs :: GuardedRhs l -> [Name l]
varsOfGRhs (GuardedRhs _ _ e) = varsOfExp e

-- | Returns variable names of an expression
varsOfExp :: Exp l -> [Name l]
varsOfExp (Let _ bind e)        = varsOfBind bind ++ varsOfExp e
varsOfExp (Lambda _ pats e)     = concatMap varsOfPat pats ++ varsOfExp e
varsOfExp (ListComp _ e qstmts) = concatMap varsOfQStmt qstmts
  where -- These functions are intended to find variables that are defined
        -- in list comprehensions and don't necessarily work for other
        -- QualStmts or Stmts!
        varsOfQStmt :: QualStmt l -> [Name l]
        varsOfQStmt (QualStmt _ stmt) = varsOfStmt stmt
        varsOfQStmt _                 = []

        varsOfStmt :: Stmt l -> [Name l]
        varsOfStmt (Generator _ pat _) = varsOfPat pat
        varsOfStmt _                   = []
varsOfExp e                     = mapOverExpRec False varsOfExp e

-- | Returns variable names of a pattern
varsOfPat :: Pat l -> [Name l]
varsOfPat p = case p of
                (PVar _ n)            -> [n]
                (PInfixApp _ p1 _ p2) -> varsOfPat p1 ++ varsOfPat p2
                (PApp _ _ pats)       -> concatMap varsOfPat pats
                (PTuple _ _ pats)     -> concatMap varsOfPat pats
                (PList _ pats)        -> concatMap varsOfPat pats
                (PParen _ pat)        -> varsOfPat pat
                (PAsPat _ _ pat)      -> varsOfPat pat
                (PIrrPat _ pat)       -> varsOfPat pat
                (PatTypeSig _ pat _)  -> varsOfPat pat
                (PViewPat _ _ pat)    -> varsOfPat pat
                (PBangPat _ pat)      -> varsOfPat pat
                _                     -> []

-- | Returns variable names of a binding
varsOfBind :: Binds l -> [Name l]
varsOfBind (BDecls _ decls) = concatMap varsOfDecl decls
varsOfBind _                = []
