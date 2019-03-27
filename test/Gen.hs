module Gen where 

import Test.QuickCheck

import Axo.ParseTree

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
genVarId = VarId <$> oneof [ onlySymbols, onlyAlphaNum ]
           where onlyAlphaNum = do
                   k <- choose (0,14)
                   (:) <$> genLowerChar <*> (vectorOf k genAlphaNumChar)
                 onlySymbols = (do
                   k <- choose (1,4)
                   vectorOf k $ elements $ punctuation++"\\") `suchThat` (/="--")

genTypeId :: Gen Identifier
genTypeId = TypeId <$> do
  k <- choose (1,10)
  f <- genUpperChar
  chs <- vectorOf (k-1) genAlphaNumChar
  return $ f:chs

genIdentifier :: Gen Identifier
genIdentifier = oneof [ genVarId, genTypeId ]

genAtom :: Gen Atom
genAtom = oneof
          [
            Id <$> genIdentifier,
            Literal <$> genLiteral
          ]

genSexp :: Int -> Gen Sexp
genSexp x = Sexp <$> genExpSeq x

genExp :: Int -> Gen Exp
genExp 0 = EAtom <$> genAtom
genExp x = frequency [ (10, EAtom <$> genAtom)
                     , (10, ESexp <$> genSexp (x `div` 2))
                     , (1, EComment <$> genComment)]

genExpSeq :: Int -> Gen ExpSeq
genExpSeq x = ExpSeq <$> (do
                           k <- choose (1,5)
                           first <- (EAtom . Id) <$> genIdentifier
                           rest <- vectorOf k $ genExp x
                           return (first:rest))
                                   

boundedSexp = sized genSexp
boundedExp = sized genExp
boundedExpSeq = sized genExpSeq

genComment :: Gen Comment
genComment = Comment <$> listOf1 genAnyChar
