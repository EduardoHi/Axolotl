module Interpreter where


import Control.Monad.State.Strict
import Data.List
import qualified Data.Map as Map
import System.Console.Repline


import Compiler (emptyState, runCompilerM, loadExpr, CompilerState)

import Axo.Eval

data InterpreterState = InterpreterState
  { _env    :: Env
  , _cState :: CompilerState
  }

initialState = InterpreterState emptyEnv emptyState

type Repl a = HaskelineT (StateT InterpreterState IO) a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = do
  env  <- gets _env
  cstate <- gets _cState
  (res, cstate') <- liftIO $ runCompilerM (loadExpr input) cstate
  case res of
    Left err -> liftIO $ putStrLn err
    Right x  -> do
      let (val,env') = runEval env x
      put $ InterpreterState {_env = env', _cState = cstate'}
      liftIO $ do
        putStrLn $ "parsed: " ++ (show x)
        putStrLn $ "evaluated: " ++ (show $ val)


-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = ["kirk", "spock", "mccoy"]
  return $ filter (isPrefixOf n) names

-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

-- | print the current environment and it's bindings
env :: [String] -> Repl ()
env _ = do
  currentEnv <- gets _env
  liftIO $ mapM_ (\(k,v) -> putStrLn $ k ++ ": " ++ (show v)) $ Map.mapWithKey (,) currentEnv

options :: [(String, [String] -> Repl ())]
options = [ ("help", help)  -- :help
          , ("env", env)    -- :env
          ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to the Axolotl Interpreter!"

repl :: IO ()
repl = flip evalStateT initialState $
       evalRepl (pure "Axo λ> ") cmd options (Just ':') (Word completer) ini
