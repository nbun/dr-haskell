f x y =  g z
  where g z = 6 + x


Prog ("") [] []
      [Func ("","f")  Public Untyped
            (Rules [Rule NoTypeAnn
                         [PVar NoTypeAnn (0,"x")
                         ,PVar NoTypeAnn (1,"y")
                         ]
                         (SimpleRhs (Apply NoTypeAnn
                                          (Symbol NoTypeAnn ("","f.g"))
                                          (List NoTypeAnn [Var NoTypeAnn (2,"z")
                                                          ,Var NoTypeAnn (0,"x")
                                                          ]
                                          )
                                     )
                          )
                          [LocalFunc ...]
              )
      ,Func (SrcSpanInfo {srcInfoSpan = SrcSpan "TypeInference/Test.hs" 2 9 2 20, srcInfoPoints = []}) (("","f.g"),SrcSpanInfo {srcInfoSpan = SrcSpan "TypeInference/Test.hs" 2 9 2 20, srcInfoPoints = []}) 2 Public Untyped (Rules [Rule (SrcSpanInfo {srcInfoSpan = SrcSpan "TypeInference/Test.hs" 2 13 2 20, srcInfoPoints = [SrcSpan "TypeInference/Test.hs" 2 13 2 14]}) NoTypeAnn [PVar NoTypeAnn ((2,"z"),SrcSpanInfo {srcInfoSpan = SrcSpan "TypeInference/Test.hs" 2 11 2 12, srcInfoPoints = []}),PVar NoTypeAnn ((0,"x"),SrcSpanInfo {srcInfoSpan = SrcSpan "TypeInference/Test.hs" 2 13 2 20, srcInfoPoints = [SrcSpan "TypeInference/Test.hs" 2 13 2 14]})] (SimpleRhs (InfixApply (SrcSpanInfo {srcInfoSpan = SrcSpan "TypeInference/Test.hs" 2 15 2 20, srcInfoPoints = []}) NoTypeAnn (Lit NoTypeAnn (Intc 6,SrcSpanInfo {srcInfoSpan = SrcSpan "TypeInference/Test.hs" 2 15 2 16, srcInfoPoints = []})) (("","+"),SrcSpanInfo {srcInfoSpan = SrcSpan "TypeInference/Test.hs" 2 15 2 20, srcInfoPoints = []}) (Var NoTypeAnn ((0,"x"),SrcSpanInfo {srcInfoSpan = SrcSpan "TypeInference/Test.hs" 2 19 2 20, srcInfoPoints = []})))) []])]
