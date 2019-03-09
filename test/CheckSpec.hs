module CheckSpec where



import Test.Hspec
import Test.QuickCheck

import Text.Megaparsec
  
import AxoParser
import AxoPrettyPrinter
import Gen


-- to use quickcheck, we need to define properties that hold true,
-- in the case of our parser, the properties that should hold true is
-- that the composition of the "parsing" and "pretty-printing" is the identity function:

-- id = parse . pretty

checkSpec :: Spec
checkSpec = do
  describe "parsing an integer literal" $ do
    it "is the inverse of pretty printing it" $ forAll genIntLit $
      (\x -> (parse intLit "" (ppLiteral x)) === (Right x))
  
-- lexerSpec :: Spec
-- lexerSpec = do
--   intSpec
--   varIdSpec


-- intSpec = do
--   describe "parseInt" $ do

--     -- Succeses
    
--     it "parses an integer literal in decimal representation" $
--       parse intLit "" "123456" `shouldParse` (IntLit "123456")

--     -- Failures

--     it "fails on a letter" $
--       parse intLit "" `shouldFailOn` "abc"


-- varIdSpec = do
--   describe "varId" $ do

--     -- Succeses
    
--     it "parses an all lowercase name" $
--       parse varId "" "abcd" `shouldParse` "abcd"

--     it "parses a camelCase name" $
--       parse varId "" "aBcD" `shouldParse` "aBcD"

--     it "cuts the parsing when it meets a reserved symbol" $
--       parse varId "" "abc(d" `shouldParse` "abc"

--     it "parses one symbol (not reserved one)" $
--       parse varId "" "+" `shouldParse` "+"

--     it "parses multiple symbols (not reserved ones)" $
--       parse varId "" ">>=" `shouldParse` ">>="

--     -- Failures

--     it "fails on a PascalCase name" $
--       parse varId "" `shouldFailOn` "Abc"




-- main :: IO ()
-- main = hspec $ do
--   describe "parseInt" $ do
--     it "parses an integer literal in decimal representation" $
--       parse intLit "" "123456" `shouldParse` (IntLit "123456")

--     it "fails on a letter" $
--       parse intLit "" `shouldFailOn` "abc"


--   describe "varId" $ do
--     it "parses an all lowercase name" $
--       parse varId "" "abcd" `shouldParse` "abcd"

--     it "parses a camelCase name" $
--       parse varId "" "aBcD" `shouldParse` "aBcD"

--     it "cuts the parsing when it meets a reserved symbol" $
--       parse varId "" "abc(d" `shouldParse` "abc"

--     it "parses one symbol (not reserved one)" $
--       parse varId "" "+" `shouldParse` "+"

--     it "parses multiple symbols (not reserved ones)" $
--       parse varId "" ">>=" `shouldParse` ">>="

--     it "fails on a PascalCase name" $
--       parse varId "" `shouldFailOn` "Abc"
