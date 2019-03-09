module CheckSpec where



import Test.Hspec
import Test.QuickCheck

import Text.Megaparsec
  
import Axo.Parser
import Axo.PrettyPrinter
import Gen


-- to use quickcheck, we need to define properties that hold true,
-- in the case of our parser, the properties that should hold true is
-- that the composition of the "parsing" and "pretty-printing" is the identity function:

-- id = parse . pretty

-- to debug one property, put the function `debug` inbetween the spec to print the cases, e.g. `debug $ forAll genIntLit ...`

checkSpec :: Spec
checkSpec = do
  describe "parsing literals" $ do
    context "parsing ints" $ do
      it "parse intLit is the inverse of ppLiteral" $ forAll genIntLit $
        (\x -> (parse intLit "" (ppLiteral x)) === (Right x))


    context "parsing floats" $ do
      it "parse floatLit is the inverse of ppLiteral" $ forAll genFloatLit $
        (\x -> (parse floatLit "" (ppLiteral x)) === (Right x))


    context "parsing strings" $ do
      it "parse stringLit is the inverse of ppLiteral" $ forAll genStringLit $
        (\x -> (parse stringLit "" (ppLiteral x)) === (Right x))
