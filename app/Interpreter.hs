{-# LANGUAGE FlexibleContexts #-}
module Interpreter where


import Control.Monad.State.Strict
import qualified Data.Map as Map
import System.Console.Repline


import Compiler (emptyState, runCompilerM, loadExpr, loadFile, CompilerState(..))

import Axo.Eval
import qualified Axo.AST as AST (Expr(..), Program(..))

data InterpreterState = InterpreterState
  { _env    :: Env
  , _cstate :: CompilerState
  }

initialState = InterpreterState emptyEnv emptyState

type Repl a = HaskelineT (StateT InterpreterState IO) a

-- Evaluation : handle each line user inputs
interpret :: String -> Repl ()
interpret input = do
  cstate <- gets _cstate
  (res, cstate') <- liftIO $ runCompilerM (loadExpr input) cstate
  modify $ \is -> is {_cstate = cstate'}
  case res of
    Left err -> liftIO $ putStrLn err
    Right (x,_)  -> do -- second argument is the type
      evalExp x

-- Tab Completion: return a completion for partial words entered
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
  (":load", fileCompleter)
  ]

completer :: (Monad m, MonadState InterpreterState m) => WordCompleter m
completer _ = do
  ns <- gets _env
  return $ boundVars ns

-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

-- | prints the type of the expression
tycheck :: [String] -> Repl ()
tycheck input = do
  cstate <- gets _cstate
  (res,_) <- liftIO $ runCompilerM (loadExpr $ unlines input) cstate
  liftIO $ case res of
             Left err -> putStrLn err
             Right (_,t) -> print t

-- | print the current environment and it's bindings
env :: [String] -> Repl ()
env [x] = do
  currentEnv <- gets _env
  liftIO $ print $ Map.lookup x currentEnv

env _ = do
  currentEnv <- gets _env
  liftIO $ mapM_ (\(k,v) -> putStrLn $ k ++ ": " ++ (show v)) $ Map.mapWithKey (,) currentEnv

tyenv :: [String] -> Repl ()
tyenv _ = do
  cstate <- gets _cstate
  let currentTypeEnv = _tyenv cstate
  liftIO $ mapM_ (\(k,v) -> putStrLn $ k ++ ": " ++ (show v)) $ Map.mapWithKey (,) currentTypeEnv

load :: [String] -> Repl ()
load args = do
  cstate <- gets _cstate
  (res,cstate') <- liftIO $ runCompilerM (loadFile $ head args) cstate
  modify $ \is -> is {_cstate = cstate'}
  case res of
    Left err -> liftIO $ putStrLn err
    Right (AST.Program exps) ->
      evalExps exps

evalExps :: [AST.Expr] -> Repl ()
evalExps exps = mapM_ evalExp exps

evalExp :: AST.Expr -> Repl ()
evalExp e = do
  env  <- gets _env
  (val,env') <- liftIO $ runEval env e
  modify $ \is -> is {_env = env'}
  liftIO $ do
    case val of
      VClosure{} -> mempty
      otherwise -> putStrLn $ "evaluated: " ++ (show val)

options :: [(String, [String] -> Repl ())]
options = [ ("help", help)    -- :help
          , ("env", env)      -- :env
          , ("t", tycheck)    -- :t
          , ("typenv", tyenv) -- :typenv
          , ("type", tycheck) -- :type
          , ("load", load)    -- :load
          ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to the Axolotl Interpreter!"

repl :: IO ()
repl = flip evalStateT initialState $
       evalRepl (pure "Axo λ> ") interpret options (Just ':') (Prefix (wordCompleter completer) defaultMatcher) ini
