-- function to run all defined tests
runAllTests :: Prelude.IO [Prelude.String]
runAllTests = do
  let tests = __allTests__
      ztests = Prelude.zip [1..] tests
      (++) = (Prelude.++)
      show = Prelude.show
  Prelude.mapM (\ (n, r) -> do
           res <- r
           Prelude.return Prelude.$ "Test " ++
             show n ++ ": " ++
               case res of
                 Tests.Success l -> "(Line " ++ show l ++ ") passed"
                 Tests.Failure l pexp o -> "(Line " ++ show l ++ ") failed: "
                                     ++ pexp ++ "\n" ++ o)
    ztests :: Prelude.IO [Prelude.String]
