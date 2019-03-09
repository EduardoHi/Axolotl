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

-- to debug one property, put the function `debug` inbetween the spec to print the cases, e.g. `debug $ forAll genIntLit ...`

checkSpec :: Spec
checkSpec = do
  describe "parsing an integer literal" $ do
    it "is the inverse of pretty printing it" $ forAll genIntLit $
      (\x -> (parse intLit "" (ppLiteral x)) === (Right x))


