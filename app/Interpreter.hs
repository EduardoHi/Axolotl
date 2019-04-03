module Interpreter where


import Control.Monad.State.Strict
import Data.List
import System.Console.Repline


import Compiler (emptyState, runCompilerM, loadExpr, CompilerState)

import Axo.Eval

data InterpreterState = InterpreterState
  { _env :: Env
  }

initialState = InterpreterState emptyEnv

type Repl a = HaskelineT (StateT InterpreterState IO) a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = do
  env <- gets _env

  liftIO $ do
    (res,finalState) <- runCompilerM (loadExpr input) emptyState
    case res of
      Left err -> putStrLn err
      Right x  -> do
        putStrLn $ "debug: "++(show x)
        print $ eval env x


-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = ["kirk", "spock", "mccoy"]
  return $ filter (isPrefixOf n) names

-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args


options :: [(String, [String] -> Repl ())]
options = [ ("help", help)  -- :help
          ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to the Axolotl Interpreter!"

repl :: IO ()
repl = flip evalStateT initialState $
       evalRepl (pure "Axo λ> ") cmd options (Just ':') (Word completer) ini
