{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# OverloadedStrings #-}

module Repl.Main (module Repl.Main) where

import Control.Lens                         hiding (Level)
import Control.Monad.Catch                  as MC
import Control.Monad.State
import Data.ByteString.Lazy                 (unpack)
import Data.Char
import Data.List
import Data.Maybe                           (fromJust, isJust)
import Data.Version                         (showVersion)
import Goodies                              (getFullPath)
import Language.Haskell.Exts                (SrcSpanInfo)
import Language.Haskell.Exts.Parser
import Language.Haskell.Interpreter
import Network.HTTP.Conduit
import Paths_drhaskell
import Repl.CmdOptions
import Repl.Loader
import Repl.Types
import StaticAnalysis.CheckState
import System.Console.Haskeline
import System.FilePath
import TypeInference.AbstractHaskell        (AHOptions (..), Expr (..),
                                             TypeAnn (..), TypeExpr (..),
                                             defaultAHOptions, showTypeExpr)
import TypeInference.AbstractHaskellGoodies (exprType', removeTypeVars)
import TypeInference.Main

{-
Current Limitations:
  - history is not used
  - no let-constructs
-}

cabalURL :: String
cabalURL =  "https://git.ps.informatik.uni-kiel.de/student-projects/mapro-2017-ss/raw/master/drhaskell.cabal"


replRead :: Repl (Maybe String)
replRead = do
  level  <- use currentLevel
  prompt <- use promptModule
  liftInput $ getInputLine $ prompt ++ " ("++ printLevel level ++")> "

replPrint :: Maybe String -> ReplInput ()
replPrint Nothing  = return ()
replPrint (Just x) = outputStrLn x

replLoop :: Repl ()
replLoop = do
  minput <- liftRepl replRead
  case minput of
       Nothing -> return ()
       Just x -> do
         (res, cont) <- replEval x
         liftInput $ replPrint res
         when cont replLoop

initInterpreter :: Repl ()
initInterpreter = do
  datadir <- liftIO getDataDir
  liftInterpreter $ Language.Haskell.Interpreter.set
    [searchPath := [".", datadir </> "TargetModules"]]
  loadInitialModules

updateCheck :: Bool -> IO (Maybe String)
updateCheck True = return Nothing
updateCheck False = MC.handleAll (\_ -> return Nothing)  $ do
  s <- simpleHttp cabalURL
  let findVersion s = head $ filter (isPrefixOf "version") (lines s)
      versionLine   = findVersion $ map (chr . fromEnum) (unpack s)
      version  = stripPrefix "version:" (filter (/= ' ') versionLine)
  return version

main :: IO ()
main = do
  initialState <- handleCmdArgs
  res <- runRepl initialState $ do
    noCheck <- use noUpdateCheck
    remoteVersion <- liftIO $ updateCheck noCheck
    liftInput (showBanner remoteVersion)
    initInterpreter
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
  _      -> (,True) <$> replEvalExp q

replHelp :: Maybe String -> Repl String
replHelp input = return $ init $ unlines $ hint [
  "Usage:",
  ":? - show this help message",
  ":l - load module",
  ":L - set level manually, disable with :L 0",
  ":q - quit",
  ":r - reload module",
  ":t - evaluate type",
  "expression - evaluate expression" ]
  where hint xs = case input of
                    Just  s -> ("Unrecognized option '" ++ s ++ "'") : xs
                    Nothing -> xs

replEvalExp :: String -> Repl (Maybe String)
replEvalExp q = case filter (not . isSpace) q of
    "" -> return Nothing
    _  ->
      MC.handleAll (\e -> do
                          liftIO $ putStrLn (displayException e)
                          return Nothing) $ do
        (errors, tExp) <- checkType q
        if isJust errors
        then return errors
        else do
          p <- use tiProg
          t <- liftInterpreter $ typeOf q
          if t == "IO ()"
            then do
              --apparently the interpreter does not put the output through 'our'
              --stdout, so we need to flush the buffer *within* the interpreter,
              --which makes this really messy. Seems to work though.
              action <- liftInterpreter $ interpret
                          ("("++q++")>>System.IO.hFlush System.IO.stdout")
                          (as :: IO ())
              liftIO action
              return Nothing
            else if not $ null p
                 then let (replaced, tExpStr) = removeTypeVars $ fromJust tExp
                          exp = if replaced
                                then "(" ++ q ++ "::" ++ showType tExpStr ++ ")"
                                else q
                      in liftInterpreter $ Just <$> eval exp
                 else liftInterpreter $ Just <$> eval q
  where
    showType = showTypeExpr defaultAHOptions
                            {unqModules = "Prelude" : unqModules defaultAHOptions}

    checkType :: String -> Repl (Maybe String, Maybe (TypeExpr SrcSpanInfo))
    checkType q = do
      p' <- use tiProg
      case p' of
           [] -> return (Nothing, Nothing)
           ps -> case parseExpWithMode (defaultParseMode{parseFilename = "<interactive>"}) q of
                      ParseFailed _ f -> return $ (Just f, Nothing)
                      ParseOk e       ->
                        case inferHSEExp ps e of
                             Left e -> return (Just
                                              $ showTIError
                                                defaultAHOptions {
                                                  unqModules =
                                                    "Prelude" :
                                                    unqModules
                                                      defaultAHOptions
                                                }
                                                e, Nothing)
                             Right e -> case fromJust $ exprType' e of
                                             t@(FuncType _ _ _) ->
                                               return (Just $
                                                 "Function with type " ++
                                                  showType t, Nothing)
                                             t -> return (Nothing, Just t)

replEvalCommand :: String -> Repl (Maybe String, Bool)
replEvalCommand cmd = if null cmd then invalid cmd else
  case head args of
    "q"      -> quit
    "quit"   -> quit
    "l"      -> load
    "load"   -> load
    "r"      -> reload
    "reload" -> reload
    "t"      -> commandTypeof args
    "type"   -> commandTypeof args
    "?"      -> help
    "h"      -> help
    "help"   -> help
    "L"      -> setLevel
    "level"  -> setLevel
    s        -> invalid s
  where args = words cmd
        quit = return (Nothing, False)
        load = case args of
          [_]       -> return (Just "No File specified", True)
          (_:_:_:_) -> return (Just "Cannot load multiple files.", True)
          [_,fn]    -> do
            fullPath <- liftIO $ getFullPath fn
            liftRepl $ modify (Control.Lens.set filename fullPath)
            errors <- loadModule fullPath
            return $ (,) (Just (unlines $ map printLoadMessage errors)) True
        reload = do
          md <- gets _filename
          if null md
          then
            return (Just "Ok, modules loaded: none.", True)
          else do
            errors <- loadModule md
            return (Just $ unlines $ map printLoadMessage errors, True)
        invalid s =  replHelp (Just s) >>= \res -> return (Just res, True)
        setLevel = let setL l = do forceLevel .= l
                                   loadInitialModules
                                   when (l == Just LevelFull) (tiProg .= [])
                                   return (Nothing, True)
                       emsg l = "Invalid level '" ++ concat (tail l)
                                ++ "' , possible values: {0, 1, 2, 3, 4}"
                   in case tail args of
                        ["0"] -> setL Nothing
                        ["1"] -> setL $ Just Level1
                        ["2"] -> setL $ Just Level2
                        ["3"] -> setL $ Just Level3
                        ["4"] -> setL $ Just LevelFull
                        ["F"] -> setL $ Just LevelFull
                        l     -> return (Just $ emsg l, True)
        help = (,True) . Just <$> replHelp Nothing

commandTypeof :: [String] -> Repl (Maybe String, Bool)
commandTypeof [_]  = return (Just "Expression expected", True)
commandTypeof args = do
    l <- use currentLevel
    case l of
         Level1    -> commandTypeofTI
         Level2    -> commandTypeofTI
         Level3    -> commandTypeofTI
         LevelFull -> commandTypeofHint
  where
    commandTypeofHint = MC.handleAll (\e ->
                          return (Just (displayException e), True)) $
                        liftInterpreter (typeOf expression) >>=
                        \res -> return (Just (expression ++
                                              " :: " ++
                                              fixType res), True)
    commandTypeofTI = do
      p' <- use tiProg
      case p' of
           [] -> return (Nothing, True)
           ps -> case parseExpWithMode (defaultParseMode{parseFilename = "<interactive>"}) expression of
                      ParseFailed _ f -> return (Just f, True)
                      ParseOk e       ->
                        case inferHSEExp ps e of
                             Left e -> return (Just $ showTIError
                                                      defaultAHOptions {
                                                        unqModules =
                                                          "Prelude" :
                                                          unqModules
                                                            defaultAHOptions
                                                      }
                                                      e, True)
                             Right e -> return (Just
                                                 (expression ++
                                                  " :: "     ++
                                                  (showTypeExpr
                                                    defaultAHOptions {
                                                      unqModules =
                                                        "Prelude" :
                                                        unqModules
                                                          defaultAHOptions
                                                    } $
                                                    fromJust         $
                                                    exprType' e)), True)
    expression = unwords $ tail args
    fixType "Prelude.Num a => a"         = "Int"
    fixType         "Num a => a"         = "Int"
    fixType "GHC.Num.Num a => a"         = "Int"
    fixType "GHC.Real.Fractional a => a" = "Float"
    fixType          "Fractional a => a" = "Float"
    fixType x                            = x

--TODO: some better ascii art?
showBanner :: Maybe String -> ReplInput ()
showBanner rv =
  let cv         = showVersion version
      -- Uses ANSI escape codes to print the text green
      msg v      = "\n\x1b[32mAn updated version " ++ "(" ++ v ++ ")"
                   ++ " of DrHaskell is available! "
                   ++ "Please update your installation.\x1b[0m"
      updateHint = case rv of
                      Just v  -> if cv < v then msg v else ""
                      Nothing -> ""
  in outputStrLn $ unlines [
  "",
  "\\ \\      DrHaskell version " ++ showVersion version,
  " \\ \\     CAU Kiel",
  " /  \\",
  "/ /\\ \\   Type \":?\" for help",
  "\n",
  "If you encounter any issues with DrHaskell, please contact us at "
  ++ "stu114713@informatik.uni-kiel.de",
  updateHint]
