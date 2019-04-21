module Interpreter where


import Control.Monad.State.Strict
import Data.List
import qualified Data.Map as Map
import System.Console.Repline


import Compiler (emptyState, runCompilerM, loadExpr, CompilerState)

import Axo.Eval

data InterpreterState = InterpreterState
  { _env    :: Env
  , _cstate :: CompilerState
  }

initialState = InterpreterState emptyEnv emptyState

type Repl a = HaskelineT (StateT InterpreterState IO) a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = do
  env  <- gets _env
  cstate <- gets _cstate
  (res, cstate') <- liftIO $ runCompilerM (loadExpr input) cstate
  modify $ \is -> is {_cstate = cstate'}
  case res of
    Left err -> liftIO $ putStrLn err
    Right (x,t)  -> do
      let (val,env') = runEval env x
      modify $ \is -> is {_env = env'}
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
env _ = do
  currentEnv <- gets _env
  liftIO $ mapM_ (\(k,v) -> putStrLn $ k ++ ": " ++ (show v)) $ Map.mapWithKey (,) currentEnv

options :: [(String, [String] -> Repl ())]
options = [ ("help", help)    -- :help
          , ("env", env)      -- :env
          , ("t", tycheck)    -- :t
          , ("type", tycheck) -- :type
          ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to the Axolotl Interpreter!"

repl :: IO ()
repl = flip evalStateT initialState $
       evalRepl (pure "Axo λ> ") cmd options (Just ':') (Word completer) ini
