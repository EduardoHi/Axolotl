module ParserSpec (
  parserSpec
  ) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
  
import Axo.Parser

parserSpec :: Spec
parserSpec = do
  atomSpec

atomSpec = do
    describe "Atom parser" $ do

       -- Succeses

        it "parses a VarId" $
            parse identifier "" "aBcD" `shouldParse` (Id (VarId "aBcD"))
        
        it "parses a TypeId" $ 
            parse identifier "" "PascalCase" `shouldParse` (Id (TypeId "PascalCase"))