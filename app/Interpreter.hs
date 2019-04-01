module Interpreter where


import Control.Monad.State.Strict
import Data.List
import System.Console.Repline


import Axo.Eval


type Repl a = HaskelineT IO a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = liftIO $ print input

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
repl = evalRepl (pure "Axo λ> ") cmd options (Just ':') (Word completer) ini
