module StaticAnalysis.StaticChecks.Select where

import           AstChecks.Check
import           Data.List
import           Data.Maybe
import           Language.Haskell.Exts
import           StaticAnalysis.Messages.StaticErrors
import           Text.EditDistance

--------------------------------------------------------------------------------
-- Find and modify (qualified) names

nameString :: Name l -> String
nameString (Ident  _ s) = s
nameString (Symbol _ s) = s

defNames :: Module l -> [Name l]
defNames m@Module{} = concatMap declName $ funBinds m ++ patBinds m
defNames _          = []

declName :: Decl l -> [Name l]
declName d = case d of
               (TypeSig _ ns _)                 -> ns
               (FunBind _ (Match _ n _ _ _ :_)) -> [n]
               (PatBind _ (PVar _ n) _ _)       -> [n] -- functions without arguments
               _                                -> []
qNameName :: QName l -> Name l
qNameName (Qual _ (ModuleName _ m) name) = name
qNameName (UnQual _ name) = name
qNameName (Special l specialcon) = Symbol l name
  where name = case specialcon of
                 UnitCon _          -> "()"
                 ListCon _          -> "[]"
                 FunCon  _          -> "->"
                 Cons    _          -> "(:)"
                 TupleCon _ _ n     -> '(' : replicate n ',' ++ ")"
                 UnboxedSingleCon _ -> "(# #)"

nameOfModule :: Module l -> Maybe (ModuleName l)
nameOfModule m@(Module _ mhead _ _ _) =
  case mhead of
    Just (ModuleHead _ mname _ _) -> Just mname
    Nothing -> Nothing

--------------------------------------------------------------------------------
-- Filter declarations

declFilter :: (Decl l -> Bool) -> Module l -> [Decl l]
declFilter p (Module _ _ _ _ decls) = filter p decls
declFilter _ _                      = []

typeSigs :: Module l -> [Decl l]
typeSigs = declFilter isTypeSig
  where
    isTypeSig TypeSig{} = True
    isTypeSig _         = False

patBinds :: Module l -> [Decl l]
patBinds = declFilter isPatBind
  where
    isPatBind PatBind{} = True
    isPatBind _         = False


funBinds :: Module l -> [Decl l]
funBinds = declFilter isFunBind
  where
    isFunBind :: Decl l -> Bool
    isFunBind FunBind{} = True
    isFunBind _         = False

--------------------------------------------------------------------------------
-- Find similar names

calcLev :: Name l -> Name l -> Int
calcLev n m = levenshteinDistance defaultEditCosts s t
  where s = nameString n
        t = nameString m

similar :: a -> (a -> [Name l]) -> Name l -> [(Name l, Int)]
similar m search n = map (\s -> (s, calcLev s n)) (search m)

similar3 :: a -> (a -> [Name l]) -> Name l -> [Name l]
similar3 m search n = take 3 $ map fst sims'
  where sims  = sortBy (\x y -> compare (snd x) (snd y)) (similar m search n)
        sims' = filter  (\x -> snd x <= nlen) sims
        nlen  = (length (nameString n) `div` 2) + 1

--------------------------------------------------------------------------------
-- Find expressions

expsOfModule :: Module l -> [Exp l]
expsOfModule (Module _ _ _ _ decls) = mapOverDecls (: []) decls

expsOfDecl :: Decl l -> [Exp l]
expsOfDecl d = mapOverDecls id [d]

qNamesOfExps :: [Exp l] -> [QName l]
qNamesOfExps exps = catMaybes $ concatMap (mapOverExp expQName) exps

expQName :: Exp l -> [Maybe (QName l)]
expQName (Var _ qn) = [Just qn]
expQName _          = [Nothing]

--------------------------------------------------------------------------------
-- Find variables

varsOfModule :: Module l -> [Name l]
varsOfModule m@(Module _ _ _ _ decls) = concatMap varsOfDecl decls

varsOfMaybeBind :: Maybe (Binds l) -> [Name l]
varsOfMaybeBind (Just bind) = varsOfBind bind
varsOfMaybeBind Nothing     = []

varsOfDecl :: Decl l -> [Name l]
varsOfDecl (FunBind _ matches)       = concatMap varsOfMatch matches
varsOfDecl (PatBind _ pat rhs mbind) =
  varsOfPat pat ++ varsOfRhs rhs ++ varsOfMaybeBind mbind
varsOfDecl _                         = []

varsOfMatch :: Match l -> [Name l]
varsOfMatch (Match _ _ pats rhs mbind) =
  concatMap varsOfPat pats ++ varsOfMaybeBind mbind ++ varsOfRhs rhs
varsOfMatch (InfixMatch _ pat _ pats rhs mbind) =
  concatMap varsOfPat (pat:pats) ++ varsOfMaybeBind mbind ++ varsOfRhs rhs

varsOfRhs :: Rhs l -> [Name l]
varsOfRhs (UnGuardedRhs _ exp) = varsOfExp exp
varsOfRhs (GuardedRhss _ grhs) = concatMap varsOfGRhs grhs

varsOfGRhs :: GuardedRhs l -> [Name l]
varsOfGRhs (GuardedRhs _ _ exp) = varsOfExp exp


varsOfExp :: Exp l -> [Name l]
varsOfExp (Let _ bind exp)    = varsOfBind bind ++ varsOfExp exp
varsOfExp (Lambda _ pats exp) = concatMap varsOfPat pats ++ varsOfExp exp
varsOfExp exp                 = mapOverExpRec False varsOfExp exp

varsOfPat :: Pat l -> [Name l]
varsOfPat p = case p of
                (PVar _ name)         -> [name]
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

varsOfBind :: Binds l -> [Name l]
varsOfBind (BDecls _ decls) = concatMap varsOfDecl decls
varsOfBind _                = []
