module Repl.Main where

import           Control.Monad.Catch                  as MC
import           Control.Monad.State
import           Data.Maybe
import           Paths_drhaskell
import           System.FilePath
import           Control.Lens hiding (Level, set)

import           Language.Haskell.Interpreter
import           System.Console.Haskeline
import           Repl.Loader
import           Repl.Types
import           Repl.CmdOptions

import           StaticAnalysis.Messages.Prettify
import           StaticAnalysis.Messages.StaticErrors

{-

Current Limitations:
  - history is not used
  - no let-constructs
  - our custom checks and modifications
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

initInterpreter :: ReplInterpreter ()
initInterpreter = do
  datadir <- liftIO getDataDir
  set [searchPath := [".", datadir </> "TargetModules"]]
  setImports ["Prelude"]

main :: IO ()
main = do
  initialState <- handleCmdArgs
  res <- runRepl initialState $ do
    liftInterpreter initInterpreter
    fname <- use filename
    when (not $ null fname) $ do
      errors <- liftRepl $ loadModule fname
      liftInput $ replPrint (Just (unlines $ map prettyError errors))
    replLoop
  case res of
       Left err -> putStrLn $ "Error:" ++ show err
       Right _  -> return ()

replEval :: String -> Repl (Maybe String)
replEval q = case q of
  ':':xs -> replEvalCommand xs
  _      -> liftInterpreter $ replEvalExp q

replHelp :: Repl String
replHelp = return $ unlines [
  ":? - This help",
  ":l - load module",
  ":r - reload module",
  ":t - evaluate type",
  "expression - evaluate expression" ]

replEvalExp :: String -> ReplInterpreter (Maybe String)
replEvalExp q = do
  MC.handleAll (\_ -> do
                      liftInput $ outputStrLn "Error!"
                      return Nothing) $ do
    t <- typeOf q
    if t == "IO ()"
      then interpret q (as :: IO ()) >>= liftIO >> return Nothing
      else Just <$> eval q

replEvalCommand :: String -> Repl (Maybe String)
replEvalCommand cmd = case cmd of
  "?" -> Just <$> replHelp
  ('l':' ': xs)-> do
    previousForceLevel <- use forceLevel
    MC.handleAll (\e -> do
                        liftInput $ outputStrLn "Could not load file"
                        liftInput $ outputStrLn $ show e
                        liftRepl $ forceLevel .= previousForceLevel
                        return Nothing) $ do
      liftRepl $ forceLevel .= Nothing
      errors <- loadModule xs
      return (Just (unlines $ map prettyError errors))
  ('r':_) -> do
    md <- gets _filename
    MC.handleAll (\_ -> do
                        liftInput $ outputStrLn "Could not load file"
                        return Nothing) $ do
      loadModule md
      return (Just "OK!")
  ('t':' ': xs) -> Just <$> liftInterpreter (typeOf xs)
