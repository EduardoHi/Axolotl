module Main where

import System.Environment

import Data.List (isPrefixOf)
import qualified Data.Set as S
import Control.Monad

import Axo.ParseTree (Program, CleanProgram)
import Axo.Parser (parseProgram, ParseError)
import Axo.ToGraph (showGraph, toGraph, ToGraph)
import Axo.Desugar (desugar)
import qualified Axo.AST as AST (toAST, Program)
import Axo.PrettyPrinter (pprint, PrettyPrint)

import Text.Megaparsec(errorBundlePretty)

data Flag
     = OGraph
     | OHaskellData
     | OAxolotlSrc

     | SDesugar
     | SAst
     | SParse

     | FlagError String
     deriving (Show, Eq, Ord)

type Flags = S.Set Flag

isSet s f = S.member f s

toFlag :: String -> Flag
toFlag s = case s of
             "-o=graph" -> OGraph
             "-o=show" -> OHaskellData
             "-o=pretty-print" -> OAxolotlSrc
             
             "-s=ast" -> SAst
             "-s=desugar" -> SDesugar
             "-s=parse" -> SParse
             s -> FlagError s

flags :: [String] -> Flags
flags = S.fromList . (map toFlag) . (filter $ isPrefixOf "-")

outputs :: (ToGraph a, Show a, PrettyPrint a) => [(Flag, a -> String)]
outputs =
  [ (OGraph, showGraph . toGraph)
  , (OHaskellData, show)
  , (OAxolotlSrc, pprint)
  ]

toDesugared :: Program -> Either ParseError CleanProgram
toDesugared p = Right $ desugar p

runToAST :: CleanProgram -> Either ParseError AST.Program
runToAST p = Right $ AST.toAST p

upToParse = parseProgram
upToDesugar = upToParse >=> toDesugared
upToAst = upToDesugar >=> runToAST

steps fs = case (isf SAst, isf SDesugar, isf SParse) of
             (True,_,_) -> (manErr fs) . upToAst
             (_,True,_) -> (manErr fs) . upToDesugar
             (_,_,True) -> (manErr fs) . upToParse
             _ -> (manErr fs) . upToParse
  where isf = isSet fs

reportError err = putStrLn $ "Error: " ++ (errorBundlePretty err)

manErr fs = either reportError
          (\program -> do
              let actions = map snd $ filter (\(x,_) -> S.member x fs) outputs
              mapM_ (\a -> putStrLn $ a program) actions)

main :: IO ()
main = do
  args <- getArgs
  sourceCode <- if null args
       then getContents -- from stdin
       else readFile $ head args -- from a file
  steps (flags args) sourceCode
  
