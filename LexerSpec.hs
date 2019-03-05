module LexerSpec (
  lexerSpec
  ) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
  
import AxoParser


lexerSpec :: Spec
lexerSpec = do
  intLitSpec
  varIdSpec


intLitSpec = do
  describe "parseInt" $ do

    -- Succeses
    
    it "parses an integer literal in decimal representation" $
      parse intLit "" "123456" `shouldParse` (IntLit "123456")

    -- Failures

    it "fails on a letter" $
      parse intLit "" `shouldFailOn` "abc"


varIdSpec = do
  describe "varId" $ do

    -- Succeses
    
    it "parses an all lowercase name" $
      parse varId "" "abcd" `shouldParse` "abcd"

    it "parses a camelCase name" $
      parse varId "" "aBcD" `shouldParse` "aBcD"

    it "cuts the parsing when it meets a reserved symbol" $
      parse varId "" "abc(d" `shouldParse` "abc"

    it "parses one symbol (not reserved one)" $
      parse varId "" "+" `shouldParse` "+"

    it "parses multiple symbols (not reserved ones)" $
      parse varId "" ">>=" `shouldParse` ">>="

    -- Failures

    it "fails on a PascalCase name" $
      parse varId "" `shouldFailOn` "Abc"
