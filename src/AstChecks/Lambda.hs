module AstChecks.Lambda where

import           Abstract
import           Control.Monad
import           Data.Foldable
import           Language.Haskell.Exts

--runCheck :: FilePath -> IO [Response SrcSpanInfo]
runCheck path =
    checkAST <$> getAST path

--checkAST :: Module l -> [Response l]
checkAST (Module _ _ _ _ q) = loopDecls q

loopDecls = map loopBinds

loopBinds (FunBind _ matches) =
    loopMatches matches
loopBinds (PatBind _ _ rhs _) =
    loopRhs rhs
loopBinds _ =
    []

loopMatches [] = []
loopMatches (Match _ _ _ rhs _:matches) =
    loopRhs rhs ++ loopMatches matches
loopMatches (InfixMatch _ _ _ _ rhs _:matches) =
    loopRhs rhs ++ loopMatches matches

loopRhs (UnGuardedRhs _ exp) =
    loopExp exp
loopRhs (GuardedRhss _ rhs) =
    loopGuardedRhs rhs

loopGuardedRhs [] = []
loopGuardedRhs (GuardedRhs _ stm exp:xs) =
    loopStm stm ++ loopExp exp ++ loopGuardedRhs xs

loopStm (Generator _ _ exp:stms) =
    loopExp exp ++ loopStm stms
loopStm (Qualifier _ exp:stms) =
    loopExp exp ++ loopStm stms
loopStm (LetStmt _ (BDecls _ decls):stms) =
    concat (loopDecls decls) ++ loopStm stms
loopStm (RecStmt _ stm:stms) =
    loopStm stm ++ loopStm stms

loopExp (Lambda info _ exp) =
    Resp "Lmabda-Function" info : loopExp exp
loopExp (InfixApp _ exp1 _ exp2) =
    loopExp exp1 ++ loopExp exp2
loopExp (App _ exp1 exp2) =
    loopExp exp1 ++ loopExp exp2
loopExp (NegApp _ exp) =
    loopExp exp
loopExp (Let _ (BDecls _ decls) exp) =
    concat (loopDecls decls) ++ loopExp exp
loopExp (If _ exp1 exp2 exp3) =
    loopExp exp1 ++ loopExp exp2 ++ loopExp exp3
loopExp (MultiIf _ rhs) =
    loopGuardedRhs rhs
loopExp (Case _ exp alternatives) =
    loopExp exp ++ loopAlternatives alternatives
loopExp (Do _ stm) =
    loopStm stm
loopExp (MDo _ stm) =
    loopStm stm
loopExp (Tuple _ _ exps) =
    loopExps exps
loopExp (TupleSection _ _ maybeExps) =
    foldr (\x xs -> case x of
                    Nothing  -> xs
                    Just exp -> loopExp exp ++ xs) [] maybeExps
loopExp (List _ exps) =
    loopExps exps
loopExp (ParArray _ exps) =
    loopExps exps
loopExp (Paren _ exp) =
    loopExp exp
loopExp (LeftSection _ exp _) =
    loopExp exp
loopExp (RightSection _ _ exp) =
    loopExp exp
loopExp (RecUpdate _ exp _) =
    loopExp exp
loopExp (EnumFrom _ exp) =
    loopExp exp
loopExp (EnumFromTo _ exp1 exp2) =
    loopExp exp1 ++ loopExp exp2
loopExp (EnumFromThenTo _ exp1 exp2 exp3) =
    loopExp exp1 ++ loopExp exp2 ++ loopExp exp3
loopExp (ParArrayFromTo _ exp1 exp2) =
    loopExp exp1 ++ loopExp exp2
loopExp (ParArrayFromThenTo _ exp1 exp2 exp3) =
    loopExp exp1 ++ loopExp exp2 ++ loopExp exp3
loopExp (ListComp _ exp qualstms) =
    loopExp exp ++ loopQualStmt qualstms
loopExp (ParComp _ exp qualStmss) =
    loopExp exp ++ loopQualStmt (concat qualStmss)
loopExp _ = []

loopExps = foldr (\x xs -> loopExp x ++ xs) []

loopQualStmt = foldr (\x xs -> case x of
    (QualStmt _ stm)           -> loopStm [stm]
    (ThenTrans _ exp)          -> loopExp exp
    (ThenBy _ exp1 exp2)       -> loopExp exp1 ++ loopExp exp2
    (GroupBy _ exp)            -> loopExp exp
    (GroupUsing _ exp)         -> loopExp exp
    (GroupByUsing _ exp1 exp2) -> loopExp exp1 ++ loopExp exp2
    ++ xs) []

loopAlternatives [] = []
loopAlternatives (Alt _ _ rhs _:alts) =
    loopRhs rhs ++ loopAlternatives alts
