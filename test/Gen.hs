module Gen where 

import Test.QuickCheck
import Control.Monad
import Numeric

import Data.Char

import Axo.Parser

-- to manually check the generators, use: `sample` e.g. `sample gentIntLit`

genIntLit :: Gen Literal
genIntLit = (IntLit . show) <$> (arbitrary :: Gen Int)

genFloatLit :: Gen Literal
genFloatLit = (FloatLit . show) <$> (arbitrary :: Gen Float)

genStringLit :: Gen Literal
genStringLit = StringLit <$> listOf genAnyChar -- this needs to have a generator for escaped chars

genCharLit :: Gen Literal
genCharLit = CharLit <$> genAnyChar

genLiteral :: Gen Literal
genLiteral = oneof
             [
               genIntLit,
               genFloatLit,
               genStringLit,
               genCharLit
             ]

genLowerChar, genUpperChar, genDigitChar :: Gen Char
genLowerChar = choose ('a','z')
genUpperChar = choose ('A','Z')
genDigitChar = choose ('0','9')

genAlphaChar, genAlphaNumChar, genPunctuationChar :: Gen Char
genAlphaChar = oneof [genLowerChar, genUpperChar]
genAlphaNumChar = oneof [genAlphaChar, genDigitChar]
genPunctuationChar = elements punctuation

punctuation = "!#$%&|*+-/:<=>?@^_~"

-- | punctuation or alphanumeric
genAnyChar :: Gen Char
genAnyChar = oneof [genPunctuationChar, genAlphaNumChar]
  

genVarId :: Gen Identifier
genVarId = VarId <$> oneof [ listOf1 $ elements $ punctuation++"\\",
                             onlyAlphaNum ]
           where onlyAlphaNum = (:) <$> genLowerChar <*> (listOf genAlphaNumChar)

genTypeId :: Gen Identifier
genTypeId = TypeId <$> do
  f <- genUpperChar
  chs <- listOf genAlphaNumChar
  return $ f:chs

genIdentifier :: Gen Identifier
genIdentifier = oneof [ genVarId, genTypeId ]

genAtom :: Gen Atom
genAtom = oneof
          [
            Id <$> genIdentifier,
            Literal <$> genLiteral
          ]

genSexp :: Gen Sexp
genSexp = Sexp <$> genExpSeq

genExp :: Gen Exp
genExp = oneof [EAtom <$> genAtom,
                ESexp <$> genSexp
                -- TODO EIexp and EInfixexp pending
               ]

genExpSeq :: Gen ExpSeq
genExpSeq =  ExpSeq <$> (listOf1 $ oneof [Left <$> genExp,
                                          Right <$> genComment])

genComment :: Gen Comment
genComment = Comment <$> do
  chs <- listOf1 genAnyChar
  return $ "--"++chs++"\n"

