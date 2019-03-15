module Spec where


-- QuickCheck Specs
import CheckSpec

import LexerSpec
import ParserSpec


import DesugarSpec
  
import Test.Hspec
  

-- Workflow to add more specs:
-- 1. decide where the spec should live, e.g. if it's a lexer spec, add it on LexerSpec.hs
-- 2. create a new spec corresponding to one function. (e.g. `intLit` has an `intLitSpec`)
-- 3. add the spec to the complete spec at the top (e.g. `lexerSpec`)


main :: IO ()
main = hspec (lexerSpec >> parserSpec >> desugarSpec >> checkSpec)
