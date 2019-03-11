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
  commentSpec
  exprSpec
  sExpSpec
  infixExpSpec

atomSpec = do
    describe "Atom parser" $ do
        context "identifiers" $ do
        -- Succeses

            it "parses a VarId" $
                parse atom "" "aBcD" `shouldParse` (Id (VarId "aBcD"))
            
            it "parses a TypeId" $ 
                parse atom "" "PascalCase" `shouldParse` (Id (TypeId "PascalCase"))

        context "literals" $ do
        -- Succeses

            it "parses an Integer" $
                parse atom "" "5" `shouldParse` (Literal (IntLit "5"))
            
            it "parses a Float" $
                parse atom "" "123.456" `shouldParse` (Literal (FloatLit "123.456"))

            it "parses a Character" $
                parse atom "" "\'a\'" `shouldParse` (Literal (CharLit 'a'))

            it "parses a String" $
                parse atom "" "\"Axolotl\"" `shouldParse` (Literal (StringLit "Axolotl"))

commentSpec = do
    describe "Comment parser" $ do

        -- Succeses

        it "parses a Comment with nothing following it" $
            parse comment "" "--\n" `shouldParse` (Comment "")

        it "parses a Comment with text following it" $
            parse comment "" "-- This is an Axo comment\n" `shouldParse` (Comment " This is an Axo comment")

        it "cuts the parsing when it meets a newline" $
            parse comment "" "-- Before newline\n After newline"  `shouldParse` (Comment " Before newline")

        it "parses a comment made out of comments" $
            parse comment "" "---------\n" `shouldParse` (Comment "-------")

exprSpec = do
    describe "Expression parser" $ do

        -- Succeses

        it "parses a VarId" $
            parse expr "" "aBcD" `shouldParse` (EAtom (Id (VarId "aBcD")))

        it "parses a TypeId" $
            parse expr "" "PascalCase" `shouldParse` (EAtom (Id (TypeId "PascalCase")))

        it "parses an Integer" $
            parse expr "" "5" `shouldParse` (EAtom (Literal (IntLit "5")))

        it "parses a VarId followed by a Literals" $
            parse expr "" "(aB \"hello\")" `shouldParse` (ESexp (Sexp (ExpSeq [
                    (Left (EAtom (Id (VarId "aB")))),
                    (Left (EAtom (Literal (StringLit "hello"))))])))
        
        it "parses a VarId followed by a Comment" $
            parse expr "" "(aB -- This is a comment\n)" `shouldParse` (ESexp (Sexp (ExpSeq [
                    (Left (EAtom (Id (VarId "aB")))),
                    (Right (Comment " This is a comment"))])))

sExpSpec = do
    describe "Symbolic Expression parser" $ do

        -- Succeses

        it "parses an sExp with a recursive sExp and a String Literal" $
            parse sExp "" "((PascalCase 2.0) \"hello\")" `shouldParse` (Sexp (ExpSeq [
                   (Left (ESexp (Sexp (ExpSeq [
                       (Left (EAtom (Id (TypeId "PascalCase")))),
                       (Left (EAtom (Literal (FloatLit "2.0"))))
                       ])))),
                   (Left (EAtom (Literal (StringLit "hello"))))]))
        
infixExpSpec = do
    describe "Infix Expression parser" $ do

        -- Succeses

        it "parses an infixExp with a variable an operator and an Integer" $
            parse infixExp "" "{x = 3}" `shouldParse` (InfixExp 
                        (EAtom (Id (VarId "x"))) 
                        (EAtom (Id (VarId "="))) 
                        (EAtom (Literal (IntLit "3"))))

        it "parses an infixExp with a variable an operator and an Integer" $
            parse infixExp "" "{5 + 18}" `shouldParse` (InfixExp 
                    (EAtom (Literal (IntLit "5"))) 
                    (EAtom (Id (VarId "+"))) 
                    (EAtom (Literal (IntLit "18"))))
        