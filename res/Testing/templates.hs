-- function to run all defined tests
runAllTests :: IO ()
runAllTests = do
  let tests = __allTests__
      ztests = zip [1..] tests
  mapM_ (\ (n, r) -> do
           res <- r
           putStrLn $ "Test " ++
             show n ++
               ": " ++
                 case res of
                     Success l -> "(Line "++show l++") passed"
                     Failure l pexp o -> "(Line "++show l++") failed ("++pexp++"): " ++ o)
        ztests