testp = Prog (("TestName"), "string") [] [] [f]

f = Func "string" (("TestName","f"), "string") 2 Public Untyped (Rules r)

r = [TypeInference.AbstractHaskell.Rule "string" NoTypeAnn [TypeInference.AbstractHaskell.PVar NoTypeAnn ((0,"x"), "string"),TypeInference.AbstractHaskell.PVar NoTypeAnn ((1,"y"),"string")] (SimpleRhs $ Apply "string" NoTypeAnn (TypeInference.AbstractHaskell.Var TypeInference.AbstractHaskell.NoTypeAnn ((2,"h"),"string")) (TypeInference.AbstractHaskell.Lit NoTypeAnn (Intc 5,"string"))) [LocalFunc t]]

t = Func "string"(("TestName","h"), "string") 1 Private Untyped (Rules s)

s = [TypeInference.AbstractHaskell.Rule "string" NoTypeAnn [TypeInference.AbstractHaskell.PVar NoTypeAnn ((3,"z"), "string")] (SimpleRhs (TypeInference.AbstractHaskell.Var NoTypeAnn ((0,"x") ,"string"))) []]

dst = (SimpleRhs $ Apply "string" NoTypeAnn (TypeInference.AbstractHaskell.Var TypeInference.AbstractHaskell.NoTypeAnn ((2,"h"),"string")) (TypeInference.AbstractHaskell.Lit NoTypeAnn (Intc 5,"string")))
