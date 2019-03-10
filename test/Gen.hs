module Gen where 

import Test.QuickCheck
import Control.Monad
import Numeric

import Data.Char

import Axo.Parser

-- to manually check the generators, use: `sample` e.g. `sample gentIntLit`

genIntLit :: Gen Literal
genIntLit = (IntLit . show) <$> (arbitrary :: Gen Int)


-- small function to avoid scientific notation while printing float value
showFullPrecision x = showFFloat Nothing x ""

genFloatLit :: Gen Literal
genFloatLit = (FloatLit . showFullPrecision) <$> (arbitrary :: Gen Float)


-- not as strong as all possible strings might be more things than alphaNumeric chars
genStringLit :: Gen Literal
genStringLit = StringLit <$> (sized $ \n -> do
                              replicateM n $ (do
                                ch <- genAnyChar
                                when (ch == '"') discard
                                return ch))


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
genPunctuationChar = elements "!#$%&|*+-/:<=>?@^_~"

-- | punctuation or alphanumeric
genAnyChar :: Gen Char
genAnyChar = oneof [genPunctuationChar, genAlphaNumChar]


genVarId :: Gen Identifier
genVarId = VarId <$> do
  f <- first
  chs <- (sized $ \n -> do
             replicateM n genAnyChar)
  return $ f:chs
  where first = oneof [genLowerChar, genPunctuationChar]

genTypeId :: Gen Identifier
genTypeId = TypeId <$> do
  f <- first
  chs <- (sized $ \n -> do
             replicateM n genAnyChar)
  return $ f:chs
  where first = genUpperChar

genIdentifier :: Gen Identifier
genIdentifier = oneof [ genVarId, genTypeId ]

genAtom :: Gen Atom
genAtom = oneof
          [
            Id <$> genIdentifier,
            Literal <$> genLiteral
          ]

