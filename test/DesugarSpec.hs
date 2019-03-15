module DesugarSpec (
  desugarSpec
  ) where

import Test.Hspec


import Axo.Desugar
import Axo.ParseTree


int123 = Literal (IntLit "123")

desugarSpec :: Spec
desugarSpec = do
  describe "Desugaring phase" $ do
    context "Program desugaring" $ do
      it "desugaring turns a program into a clean Program" $ do
         desugar (Program
                   [(Left
                     (EAtom int123))])
           `shouldBe`
           CleanProgram [CleanEAtom int123]

    
