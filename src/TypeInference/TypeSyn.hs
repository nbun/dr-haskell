module TypeInference.TypeSyn (collectTypeSyns) where

import TypeInference.AbstractHaskell


collectTypeSyns :: Prog l -> Prog l
collectTypeSyns (Prog mn imps tdecls fdecls) =
  let lts = (collect tdecls)
      replts = (replaceTypeSyns (simple lts) (simple lts))
      nf = backToAHForm replts tdecls -- TypeDecl l
      newP = Prog mn imps nf fdecls
      tstProg = transformTypeSig replts newP
      in tstProg--tstProg--Prog mn imps nf fdecls

transformTypeSig :: [(QName,TypeExpr l)] -> Prog l -> Prog l
transformTypeSig ntd (Prog mn imps tdecls fdecls) =
  Prog mn imps tdecls $ map (transformTypeSigFDecl ntd) fdecls

transformTypeSigFDecl :: [(QName,TypeExpr a)] -> FuncDecl a -> FuncDecl a
transformTypeSigFDecl ntd (Func a b c d tsig rls) =
  Func a b c d (transformSig ntd tsig) rls

transformSig :: [(QName,TypeExpr a)] -> TypeSig a -> TypeSig a
transformSig _ Untyped                       = Untyped
transformSig [] z@(TypeSig (TVar ((a,b), c)))  |  b == "String" = TypeSig (TCons c (("Prelude","[]"),c) [TCons c (("Prelude","Char"),c) []])
                                               | otherwise = z
transformSig [] z@(TypeSig(TCons a (qn@(e,g),b) ts)) | g == "String" = TypeSig (TCons b (("Prelude","[]"),b) [TCons b (("Prelude","Char"),b) []])
                                                     | otherwise = z
transformSig [] z@(TypeSig (FuncType a t1 t2)) = let TypeSig t1' = transformSig [] (TypeSig t1)
                                                     TypeSig t2' = transformSig [] (TypeSig t2)
                                                     in TypeSig (FuncType a t1' t2')
transformSig ((x,y):xs) z@(TypeSig (TVar ((a,b), c))) | (snd x) == b = TypeSig y
                                                      | b == "String" = TypeSig (TCons c (("Prelude","[]"),c) [TCons c (("Prelude","Char"),c) []])
                                                      | otherwise  = transformSig xs z
transformSig ((x,y):xs) z@(TypeSig(TCons a (qn@(e,g),b) ts))| x == qn = TypeSig y
                                                            | g == "String" = TypeSig (TCons b (("Prelude","[]"),b) [TCons b (("Prelude","Char"),b) []])
                                                            | otherwise = transformSig xs z
transformSig l@((x,y):xs) z@(TypeSig (FuncType a t1 t2)) =
  let TypeSig t1' = transformSig l (TypeSig t1)
      TypeSig t2' = transformSig l (TypeSig t2)
      in TypeSig (FuncType a t1' t2')

collect :: [TypeDecl l] -> [TypeDecl l]
collect []                         = []
collect ((Type _ _ _ _ _):xs)      = collect xs
collect (a@(TypeSyn _ _ _ _ _):xs) = a : collect xs

simple :: [TypeDecl a] -> [(QName,TypeExpr a)]
simple [] = []
simple ((TypeSyn _ (qn, _) _ _ v):xs) =
  (qn, v):simple xs

replaceTypeSyns ::
  [(QName,TypeExpr a)]-> [(QName,TypeExpr a)] -> [(QName,TypeExpr a)]
replaceTypeSyns [] _ = []
replaceTypeSyns ((x,i@(FuncType a t1 t2 )):xs) xys =
  let ((_,t1'):_) = replaceTypeSyns [(x,t1)] xys
      ((_,t2'):_) = replaceTypeSyns [(x,t2)] xys
      in (x,(FuncType a t1' t2')) : replaceTypeSyns xs xys
replaceTypeSyns ((x,y):xs) xys =
  case checkTuple (x,y) xys of
    (Just (z,t)) -> replaceTypeSyns ((x,t):xs) xys
    Nothing      -> (x,y) : replaceTypeSyns xs xys

checkTuple ::
  (QName,TypeExpr a) -> [(QName,TypeExpr a)] -> Maybe (QName,TypeExpr a)
checkTuple  (x,y) [] = Nothing
checkTuple (x,y) ((s,r):ys) | checkTyExprQn y s     == True = Just (s,r)
                            | otherwise             = checkTuple (x,y) ys

checkTyExprQn :: TypeExpr a -> QName -> Bool
checkTyExprQn (TVar (v,_))         (_,qn) | snd v == qn = True
                                          | otherwise   = False
checkTyExprQn (FuncType _ t1 t2 )    (_,qn) = False
checkTyExprQn (TCons _ (qn',a) t) (_,qn) | (snd qn') == qn = True
                                           | otherwise = False

backToAHForm :: [(QName,TypeExpr a)] -> [TypeDecl a] -> [TypeDecl a]
backToAHForm [] _ = []
backToAHForm a@((x,y):xs) ts = map (checkForOneTD a) ts

checkForOneTD :: [(QName,TypeExpr a)] -> TypeDecl a -> TypeDecl a
checkForOneTD [] f = f
checkForOneTD ((x,y):xs) f@(TypeSyn a (qn, d) b c t) | x == qn = TypeSyn a (qn,d) b c y
                                                   | otherwise = checkForOneTD xs f
