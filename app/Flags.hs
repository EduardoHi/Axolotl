module Flags where

import Data.List (isPrefixOf)
import qualified Data.Set as S

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
