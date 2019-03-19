module DesugarSpec (
  desugarSpec
  ) where

import Test.Hspec


import Axo.Desugar
import Axo.ParseTree


-- elements to build test cases
int123 = Literal (IntLit "123")

desugarSpec :: Spec
desugarSpec = do
  describe "Desugaring phase" $ do
    context "Program desugaring" $ do
      it "desugaring turns a program into a clean Program" $ do
         desugar (Program
                   [EAtom int123])
           `shouldBe`
           CleanProgram [CleanEAtom int123]


    context "Indent expression desugaring" $ do
      it "desugars to a sexp" $ do
        pending

    context "Indent expression desugaring" $ do
      it "desugars to a sexp" $ do
        pending

    context "Infix expression desugaring" $ do
      it "desugars to a sexp" $ do
        pending

    context "Sexp desugaring" $ do
      it "desugars to a clean sexp(it's subexpressions desugared)" $ do
        pending


    context "Expression desugaring" $ do
      it "desugars a sexp" $ do
        pending

      it "desugars an atom" $ do
        pending

      it "desugars an indent exp" $ do
        pending

      it "desugars an infix exp" $ do
        pending        

