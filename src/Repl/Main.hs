module Repl.Main where

import           Control.Monad.Catch
import           Control.Monad.State
import           System.Console.Haskeline

import           Language.Haskell.Interpreter

{-

Current Limitations:
  - no error handling
  - history is not used
  - no let-constructs
  - no error handling :/
  - module reloading
  - anything, really
-}

type ReplInput = InputT IO
type ReplInterpreter = InterpreterT ReplInput
--type ReplState = StateT () (ReplInterpreter)


-- no idea if these instances are valid
-- they work in my small tests
instance (MonadIO m, MonadThrow m) => MonadThrow (InputT m) where
  throwM = throwIO

instance (MonadException m, MonadCatch m) => MonadCatch (InputT m) where
  catch a h = System.Console.Haskeline.catch a h

instance (MonadException m, MonadMask m) => MonadMask (InputT m) where
  mask a = lift $ mask $ \u -> runInputT defaultSettings (a (\b -> lift $ u (runInputT defaultSettings b)))
  uninterruptibleMask a = lift $ uninterruptibleMask $ \u -> runInputT defaultSettings (a (\b -> lift $ u (runInputT defaultSettings b)))



replRead :: ReplInput (Maybe String)
replRead = getInputLine "Dr. Haskell> "
replPrint :: Maybe String -> ReplInput ()
replPrint Nothing  = return ()
replPrint (Just x) = outputStrLn x
replLoop :: ReplInterpreter ()
replLoop = do
  minput <- lift replRead
  case minput of
       Nothing -> return ()
       Just x -> do
         res <- replEval x
         lift $ replPrint res
         replLoop

main :: IO ()
main = do
  res <- (runInputT defaultSettings $ runInterpreter replLoop)
  case res of
       Left err -> putStrLn $ "Error:" ++ (show err)
       Right _  -> return ()

replEval :: String -> ReplInterpreter (Maybe String)
replEval q = case q of
  ":?" -> Just <$> replHelp
  (':':'l':' ': xs)-> do
    loadModules [xs]
    setTopLevelModules ["Main"]
    return $ Just "OK!"
  (':':'t':' ': xs) -> Just <$> typeOf xs
  _    -> replEvalExp q
    --Just <$> eval q
    --interpret q (as :: IO ()) >>= liftIO
    --return ""

replHelp :: ReplInterpreter String
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
