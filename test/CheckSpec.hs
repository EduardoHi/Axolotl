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
    genSpec "int" genIntLit intLit ppLiteral
    genSpec "float" genFloatLit floatLit ppLiteral
    genSpec "string" genStringLit stringLit ppLiteral
    genSpec "int" genCharLit charLit ppLiteral

  describe "parsing identifiers" $ do
    genSpec "varId" genVarId varId ppIdentifier

    genSpec "typeId" genTypeId typeId ppIdentifier

  describe "parsing atoms" $ do
    genSpec "atom" genAtom atom ppAtom

  describe "parsing Expressions" $ do
    genSpec "expression" genExp expr pprint
    genSpec "expression sequence" genExpSeq expSeq ppExpSeq
    genSpec "comment" genComment comment pprint
    genSpec "s-expression" genSexp sExp pprint


-- | genSpec needs a name, a generator, a parser and a pretty printer,
-- | all of which must act on the same typep
genSpec :: (Show a, Eq a) => String -> (Gen a) -> (Parser a) -> (a -> String) -> SpecWith ()
genSpec name gen parser pretty = do
    it ("parsing "++name++" is the inverse of pretty printing it") $ forAll gen $
      (\x -> (parse parser "" (pretty x)) === (Right x))
  
