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


-- not as strong as all possible strings might be more things than alphaNumeric chars
genStringLit :: Gen Literal
genStringLit = StringLit <$> listOf genAnyChar

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
genPunctuationChar = elements "!#$%&|*+-/:<=>?@^_~\\"

-- | punctuation or alphanumeric
genAnyChar :: Gen Char
genAnyChar = oneof [genPunctuationChar, genAlphaNumChar]
  

genVarId :: Gen Identifier
genVarId = VarId <$> oneof [ listOf1 genPunctuationChar,
                             onlyAlphaNum ]
           where onlyAlphaNum = (:) <$> genLowerChar <*> (listOf genAlphaNumChar)
  -- f <- first
  -- chs <- listOf1 genAnyChar
  -- return $ f:chs
  -- where first = oneof [genLowerChar, genPunctuationChar]

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

