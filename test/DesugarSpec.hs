module DesugarSpec (
  desugarSpec
  ) where

import Test.Hspec


import Axo.Desugar
import Axo.ParseTree


-- elements to build test cases
int123 = Literal (IntLit "123")
stringInfix = (InfixExp 
                        (EAtom (Id (VarId "x"))) 
                        (EAtom (Id (VarId "="))) 
                        (EAtom (Literal (IntLit "3"))))
                        
stringSExp = (CleanSexp [
                    (CleanVar "="),
                    (CleanVar "x"),
                    (CleanLit (IntLit "3"))])

desugarSpec :: Spec
desugarSpec = do
  describe "Desugaring phase" $ do
    context "Program desugaring" $ do
      it "desugaring turns a program into a clean Program" $ do
         desugar (Program
                   [EAtom int123])
           `shouldBe`
           CleanProgram [CleanLit (IntLit "123")]

    context "Infix expression desugaring" $ do
      it "desugars to a sexp" $ do
        desugar stringInfix
          `shouldBe` stringSExp

    context "Sexp desugaring" $ do
      it "desugars to a clean sexp(it's subexpressions desugared)" $ do
        pending

    context "Expression desugaring" $ do
      it "desugars a sexp" $ do
        pending

      it "desugars an atom" $ do
        pending

      it "desugars an infix exp" $ do
        pending        

