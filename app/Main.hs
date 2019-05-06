module Main where

import System.Environment

import Compiler(runCompilerM, loadModule, emptyState, CompilerState(..))

import Interpreter(repl)

import Flags(flags)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then repl
    else compile args

compile :: [String] -> IO ()
compile args = do
  let filename = head args
  sourceCode <- readFile $ filename
  let initialState =  emptyState
        { _flags = (flags args)
        , _filename = (takeWhile (/='.') filename)
        }
  (res, _) <- runCompilerM (loadModule sourceCode) initialState -- second element is finalState
  case res of
    Left err -> putStrLn err
    _ -> print res
  return ()
