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
                     Success -> "passed"
                     Failure o -> "failed: " ++ o)
        ztests