module LexerSpec (
  lexerSpec
  ) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
  
import Axo.Parser

lexerSpec :: Spec
lexerSpec = do
  intSpec
  floatSpec
  charSpec
  stringSpec
  varIdSpec
  typeIdSpec

intSpec = do
  describe "Integer parser" $ do

    -- Succeses
    
    it "parses an integer literal in decimal representation" $
      parse intLit "" "123456" `shouldParse` (IntLit "123456")

    -- Failures

    it "fails on a letter" $
      parse intLit "" `shouldFailOn` "abc"

floatSpec = do
  describe "Float parser" $ do

    -- Succeses

    it "parses a float literal in decimal representation" $
      parse floatLit "" "123.456" `shouldParse` (FloatLit "123.456")
    
    it "parses the float literal 2.0" $
      parse floatLit "" "2.0"  `shouldParse` (FloatLit "2.0")

    -- Failures

    it "fails on integer" $
      parse floatLit "" `shouldFailOn` "2"

charSpec = do
  describe "Character parser" $ do

    -- Succeses

    it "parses an alphabetic character" $
      parse charLit "" "\'a\'" `shouldParse` (CharLit 'a')

    it "parses a numeric character" $
      parse charLit "" "\'4\'" `shouldParse` (CharLit '4')

    it "parses a non alphanumeric character" $
      parse charLit "" "\'%\'" `shouldParse` (CharLit '%')

    -- Failures

    it "fails on more than one character" $
      parse charLit "" `shouldFailOn` "a!"

    it "fails if there is no closing single quote" $
      parse charLit ""  `shouldFailOn` "\' "
      
stringSpec = do
  describe "String parser" $ do

    -- Succeses

    it "parses a string with all uppercase letters" $
      parse stringLit "" "\"AXOLOTL\"" `shouldParse` (StringLit "AXOLOTL")

    it "parses a string with all lowercase letters" $
      parse stringLit "" "\"this is a lowercase string\"" `shouldParse` (StringLit "this is a lowercase string")

    it "parses a string with only non alphanumeric characters" $
      parse stringLit "" "\"!@#$%^&\"" `shouldParse` (StringLit "!@#$%^&")
    
    it "parses a string with the parenthises characters" $
      parse stringLit "" "\"() {}\"" `shouldParse` (StringLit "() {}")

    it "parses a string with escaped characters" $
      parse stringLit "" "\"There is an escaped char \n\""  `shouldParse` (StringLit "There is an escaped char \n")

    -- Failures

    it "fails if there are no closing quotes" $
      parse stringLit "" `shouldFailOn` "\" " 


varIdSpec = do
  describe "VarId parser" $ do

    -- Succeses
    
    it "parses an all lowercase name" $
      parse varId "" "abcd" `shouldParse` (VarId "abcd")

    it "parses a camelCase name" $
      parse varId "" "aBcD" `shouldParse` (VarId "aBcD")

    it "cuts the parsing when it meets a reserved symbol" $
      parse varId "" "abc(d" `shouldParse` (VarId "abc")

    it "parses one symbol (not reserved one)" $
      parse varId "" "+" `shouldParse` (VarId "+")

    it "parses multiple symbols (not reserved ones)" $
      parse varId "" ">>=" `shouldParse` (VarId ">>=")

    -- Failures

    it "fails on a PascalCase name" $
      parse varId "" `shouldFailOn` "Abc"

    it "fails if name starts with a number" $
      parse varId "" `shouldFailOn` "4abc"
    
    it "fails if there are spaces in the string" $
      parse varId "" `shouldFailOn` "Space here"
    
    it "fails if there are combined alphanumeric and non-alphanumeric characters" $
      parse varId "" `shouldFailOn` "a#c!"


typeIdSpec = do
  describe "TypeId parser" $ do

  -- Succeses

    it "parses an all uppercase name" $
      parse typeId "" "ABCD" `shouldParse` (TypeId "ABCD")

    it "parses a PascalCase name" $
      parse typeId "" "PascalCase" `shouldParse` (TypeId "PascalCase")

    it "cuts the parsing when it meets a reserved symbol" $
      parse typeId "" "Abc)d" `shouldParse` (TypeId "Abc")

    -- Failures

    it "fails if name starts with a number" $
      parse typeId "" `shouldFailOn` "4abc"
    
    it "fails if name begins with a non-alphanumeric character" $
      parse typeId "" `shouldFailOn` "#Abc"

    it "fails if name contains a non-alphanumeric character" $
      parse typeId "" `shouldFailOn` "Ab#c"

    it "fails if there are spaces in the string" $
      parse typeId "" `shouldFailOn` "Space here"
  