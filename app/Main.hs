module Main where

import System.Environment

import Axo.Parser (parseProgram)

main :: IO ()
main = do
  args <- getArgs
  s <- if null args
       then getContents
       else readFile $ head args
  putStrLn $ parseProgram s
