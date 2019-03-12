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
    genSpec "int" genIntLit intLit
    genSpec "float" genFloatLit floatLit
    genSpec "string" genStringLit stringLit
    genSpec "char" genCharLit charLit

  describe "parsing identifiers" $ do
    genSpec "varId" genVarId varId
    genSpec "typeId" genTypeId typeId

  describe "parsing atoms" $ do
    genSpec "atom" genAtom atom

  describe "parsing comments" $ do
    genSpec "comment" genComment comment

  describe "parsing Expressions" $ do
    genSpec "expression" boundedExp expr
    genSpec "a sequence of expressions" boundedExpSeq expSeq
    genSpec "s-expression" boundedSexp sExp


-- | genSpec needs a name, a generator, a parser and a pretty printer,
-- | all of which must act on the same type
genSpec :: (Show a, Eq a, PrettyPrint a) => String -> (Gen a) -> (Parser a) -> SpecWith ()
genSpec name gen parser = do
    it ("parsing "++name++" is the inverse of pretty printing it") $ forAll gen $
      (\x -> (parse parser "" (pprint x)) === (Right x))
  
