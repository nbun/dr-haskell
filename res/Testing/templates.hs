-- function to run all defined tests
runAllTests :: Prelude.IO [Prelude.String]
runAllTests = do
  let tests = __allTests__
      ztests = Prelude.zip [1..] tests
      (++) = (Prelude.++)
      show = Prelude.show
  (Prelude.mapM (\ (n, r) -> do
           res <- r
           Prelude.return Prelude.$ "Test " ++
             Prelude.show n ++
               ": " ++
                 case res of
                     Success l -> "(Line "++show l++") passed"
                     Failure l pexp o -> "(Line "++show l++") failed: "++pexp++"\n" ++ o)
        ztests) :: Prelude.IO [Prelude.String]
