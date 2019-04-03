module Spec where


-- QuickCheck Specs
import CheckSpec
import Gen
import LexerSpec
import ParserSpec
import DesugarSpec

import Axo.Parser
import Axo.PrettyPrinter

import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec


-- Workflow to add more specs:
-- 1. decide where the spec should live, e.g. if it's a lexer spec, add it on LexerSpec.hs
-- 2. create a new spec corresponding to one function. (e.g. `intLit` has an `intLitSpec`)
-- 3. add the spec to the complete spec at the top (e.g. `lexerSpec`)


main :: IO ()
main = hspec (lexerSpec >> parserSpec >> desugarSpec >> checkSpec) >> roundTrip
    where
        roundTrip :: IO ()
        roundTrip =
            quickCheckWith stdArgs { maxSuccess = 4000 } $
            -- TODO: remove boundedExp and change for program
            forAll boundedExp $ \expression ->
            (parse expr "" (prettyText expression)) === (Right expression)
