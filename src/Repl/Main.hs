module Repl.Main where

import           System.FilePath
import           Paths_drhaskell

import           System.Console.Haskeline
import           Language.Haskell.Interpreter
import           Repl.Types
import           Repl.Loader

{-

Current Limitations:
  - no error handling
  - history is not used
  - no let-constructs
  - no error handling :/
  - module reloading
  - anything, really
-}



replRead :: ReplInput (Maybe String)
replRead = getInputLine "Dr. Haskell> "
replPrint :: Maybe String -> ReplInput ()
replPrint Nothing  = return ()
replPrint (Just x) = outputStrLn x
replLoop :: Repl ()
replLoop = do
  minput <- liftInput replRead
  case minput of
       Nothing -> return ()
       Just x -> do
         res <- replEval x
         liftInput $ replPrint res
         replLoop

main :: IO ()
main = do
  res <- runRepl $ do
    datadir <- liftIO getDataDir
    liftInterpreter $ set [searchPath := [".", datadir </> "TargetModules"]]
    replLoop
  case res of
       Left err -> putStrLn $ "Error:" ++ (show err)
       Right _  -> return ()

replEval :: String -> Repl (Maybe String)
replEval q = case q of
  ":?" -> Just <$> replHelp
  (':':'l':' ': xs)-> do
    loadModule xs
    return $ Just "OK!"
  (':':'t':' ': xs) -> Just <$> (liftInterpreter $ typeOf xs)
  _    -> liftInterpreter $ replEvalExp q
    --Just <$> eval q
    --interpret q (as :: IO ()) >>= liftIO
    --return ""

replHelp :: Repl String
replHelp = return $ unlines [
  ":? - This help",
  ":l - load module",
  ":t - evaluate type",
  "expression - evaluate expression" ]

replEvalExp :: String -> ReplInterpreter (Maybe String)
replEvalExp q = do
  t <- typeOf q
  if t == "IO ()"
     then interpret q (as :: IO ()) >>= liftIO >> return Nothing
     else Just <$> eval q
