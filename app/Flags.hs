module Flags where

import Data.List (isPrefixOf)
import qualified Data.Set as S

data Flag
     = OGraph
     | OHaskellData
     | OAxolotlSrc
     | OAll

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
             "-o=all" -> OAll
             
             "-s=ast" -> SAst
             "-s=desugar" -> SDesugar
             "-s=parse" -> SParse
             s -> FlagError s


flags :: [String] -> Flags
flags names = normalize $ mkflags names
  where mkflags = S.fromList . (map toFlag) . (filter $ isPrefixOf "-")

-- | normalize "expands" each flag of the flags set into equivalent flags.
-- | for example, -o=all expand into every -o flag
normalize :: Flags -> Flags
normalize fs =
  S.foldl S.union S.empty $ S.map expand fs
  where expand flag = case flag of
                        OAll -> S.fromList [OGraph, OHaskellData, OAxolotlSrc]
                        x -> S.singleton x
