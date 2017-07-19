{-# LANGUAGE TupleSections #-}

module Repl.Main (module Repl.Main) where

import           Control.Lens                 hiding (Level)
import           Control.Monad.Catch          as MC
import           Control.Monad.State
import           Paths_drhaskell
import           System.FilePath

import           Language.Haskell.Interpreter
import           Repl.CmdOptions
import           Repl.Loader
import           Repl.Types
import           System.Console.Haskeline

{-
Current Limitations:
  - history is not used
  - no let-constructs
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
         (res, cont) <- replEval x
         liftInput $ replPrint res
         when cont replLoop

initInterpreter :: ReplInterpreter ()
initInterpreter = do
  datadir <- liftIO getDataDir
  Language.Haskell.Interpreter.set
    [searchPath := [".", datadir </> "TargetModules"]]
  setImportsQ [("Prelude", Nothing),
               ("System.IO", Just "System.IO")] --needed for hFlush and stdout
                                                --for proper putStrLn

main :: IO ()
main = do
  initialState <- handleCmdArgs
  res <- runRepl initialState $ do
    liftInterpreter initInterpreter
    fname <- use filename
    unless (null fname) $ do
      errors <- liftRepl $ loadModule fname
      liftInput $ replPrint (Just (unlines $ map printLoadMessage errors))
    replLoop
  case res of
       Left err -> putStrLn $ "Error:" ++ show err
       Right _  -> return ()

replEval :: String -> Repl (Maybe String, Bool)
replEval q = case q of
  ':':xs -> replEvalCommand xs
  _      -> liftInterpreter $ (,True) <$> replEvalExp q

replHelp :: Maybe String -> Repl String
replHelp input = return $ unlines $ hint : [
  ":? - This help",
  ":l - load module",
  ":r - reload module",
  ":t - evaluate type",
  "expression - evaluate expression" ]
  where hint = case input of
                 Just  s -> "Unrecognized option '" ++ s ++ "'"
                 Nothing -> ""

replEvalExp :: String -> ReplInterpreter (Maybe String)
replEvalExp q =
  MC.handleAll (\e -> do
                      liftIO $ putStrLn (displayException e)
                      return Nothing) $ do
    t <- typeOf q
    if t == "IO ()"
      then do
        --apparently the interpreter does not put the output through 'our'
        --stdout, so we need to flush the buffer *within* the interpreter,
        --which makes this really messy. Seems to work though.
        action <- interpret
                    ("("++q++")>>System.IO.hFlush System.IO.stdout")
                    (as :: IO ())
        liftIO action
        return Nothing
      else Just <$> eval q

replEvalCommand :: String -> Repl (Maybe String, Bool)
replEvalCommand cmd = if null cmd then invalid cmd else
  case head args of
    "q"      -> quit
    "quit"   -> quit
    "l"      -> load
    "load"   -> load
    "r"      -> reload
    "reload" -> reload
    "t"      -> typeof
    "type"   -> typeof
    s        -> invalid s
  where args = words cmd
        quit = return (Nothing, False)
        load = let fn = args !! 1 in do
          previousForceLevel <- use forceLevel
          liftRepl $ forceLevel .= previousForceLevel
          liftRepl $ modify (Control.Lens.set filename fn)
          errors <- loadModule fn
          return $ (,) (Just (unlines $ map printLoadMessage errors)) True
        reload = do
          md <- gets _filename
          errors <- loadModule md
          return $ (,) (Just (unlines $ map printLoadMessage errors)) True
        typeof =  liftInterpreter (typeOf $ args !! 1) >>=
                                  \res -> return (Just res, True)
        invalid s =  replHelp (Just s) >>= \res -> return (Just res, True)
