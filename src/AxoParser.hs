module AxoParser where

import System.Environment

import Control.Monad
import Control.Monad.Except
import Control.Applicative hiding (many, some)

import Data.Void
import Numeric
import Data.Text hiding (empty)
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String


data Literal = IntLit String
             | FloatLit String
             | StringLit String
             | CharLit Char
             deriving (Show)


data Program = Program [Sexp]

data Atom = Id String
          | Literal Literal
          deriving (Show)

data Sexp = Sexp ExpSeq deriving (Show)

data Exp = ESexp Sexp
         | EAtom Atom
         | EIexp Iexp
         | EInfixexp InfixExp
         deriving (Show)

data ExpSeq = ExpSeq [Exp] deriving (Show)

data Iexp = Iexp ExpSeq [ExpSeq] deriving (Show)

data InfixExp = InfixExp Exp Exp Exp deriving (Show)


-------- Lexer --------


-- | this is a non-permissive space consumer, it only consumes 1 space char
sc :: Parser ()
sc = L.space oneSpace empty empty

oneSpace = char ' ' >> return ()


surroundedBy :: Parser b -> Parser a -> Parser a
surroundedBy by = between by by


lexeme = L.lexeme sc

-- Helpers (i.e. not part of the "tokens")

letter :: Parser Char
letter = letterChar

digit :: Parser Char
digit = digitChar

decimal :: Parser String
decimal = some digitChar

reservedSymbols = ['(', ')', '{', '}']

validSymbol = noneOf reservedSymbols

notDoubleQuote = noneOf ['"']

-- Lexemes 

identifier :: Parser Atom
identifier = Id <$> (varId <|> typeId)

varId :: Parser String
varId = do
  f <- lowerChar
  rest <- many validSymbol
  return $ f:rest

typeId :: Parser String
typeId = do
  f <- upperChar
  rest <- many validSymbol
  return $ f:rest

intLit :: Parser Literal
intLit = IntLit <$> decimal

floatLit :: Parser Literal
floatLit = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  return $ FloatLit $ d1 ++ "." ++ d2

-- this needs to handle more cases, especially the char escapes
stringLit :: Parser Literal
stringLit = StringLit <$> surroundedBy doubleQuote (many notDoubleQuote)
  where doubleQuote = char '"'

charLit :: Parser Literal
charLit = CharLit <$> surroundedBy singleQuote alphaNumChar
  where singleQuote = char '\''

-------- Parser --------

program :: Parser Program
program = Program <$> many sExp

sExp :: Parser Sexp
sExp = Sexp <$> between (char '(') (char ')') expSeq

expSeq :: Parser ExpSeq
expSeq = ExpSeq <$> sepBy1 expr oneSpace

expr :: Parser Exp
expr = (ESexp <$> sExp) <|> (EAtom <$> atom) -- <|> i

atom :: Parser Atom
atom = identifier <|> (Literal <$> literal)

literal :: Parser Literal
literal = intLit <|> floatLit <|> stringLit <|> charLit

