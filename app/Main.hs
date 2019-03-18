module Main where

import System.Environment

import Data.List (isPrefixOf)
import qualified Data.Set as S

import Axo.Parser (parseProgram, runParser)
import Axo.ToGraph (showGraph, toGraph)


data Flag
     = OGraph
     | OHaskellData
     | OAxolotlSrc
     | OError String
     deriving (Show, Eq, Ord)

type Flags = S.Set Flag

isSet s f = S.member f s

toFlag :: String -> Flag
toFlag s = case s of
             "-g" -> OGraph
             "--graph" -> OGraph
             "--haskell-data" -> OHaskellData
             "--axolotl-src" -> OAxolotlSrc
             s -> OError s

flags :: [String] -> Flags
flags = S.fromList . (map toFlag) . (filter $ isPrefixOf "-")

main :: IO ()
main = do
  args <- getArgs
  let flagsSet = flags args
  let graphIsSet = isSet flagsSet OGraph
  s <- if null args
       then getContents -- from stdin
       else readFile $ head args -- from a file
  putStrLn $ runParser s (if graphIsSet
                          then (showGraph . toGraph)
                          else show)
    
