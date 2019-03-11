{-# LANGUAGE OverloadedStrings #-}
module Axo.Parser where

import System.Environment

import Control.Monad
import Control.Monad.Except
import Control.Applicative hiding (many, some)

import Data.Void
import Data.Char
import Numeric
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Literal = IntLit String
             | FloatLit String
             | StringLit String
             | CharLit Char
             deriving (Show, Eq)


-- TODO: if we relax the Program definition of not only one space, and
-- not only sexps, Program and ExpSeq are equivalent
-- and things like comment handling is easier
data Program = Program [Either Sexp Comment] deriving (Show, Eq)

data Identifier = VarId String
                | TypeId String
                deriving (Show, Eq)

data Atom = Id Identifier
          | Literal Literal
          deriving (Show, Eq)

data Sexp = Sexp ExpSeq deriving (Show, Eq)

data Exp = ESexp Sexp
         | EAtom Atom
         | EIexp Iexp
         | EInfixexp InfixExp
         deriving (Show, Eq)

data ExpSeq = ExpSeq [Either Exp Comment] deriving (Show, Eq)

data Iexp = Iexp ExpSeq [ExpSeq] deriving (Show, Eq)

data InfixExp = InfixExp Exp Exp Exp deriving (Show, Eq)

-- Comments might appear inside a sequence of expressions, or at the top level of a program
data Comment = Comment String deriving (Show, Eq)

-------- Lexer --------

-- | this is a non-permissive space consumer, it only consumes 1 space char
sc :: Parser ()
sc = L.space oneSpace empty empty

oneSpace = char ' ' >> return ()


-- | equivalent to `between by by`
surroundedBy :: Parser b -> Parser a -> Parser a
surroundedBy by = between by by


lexeme = L.lexeme sc

-- Helpers (i.e. not part of the "tokens")

-- | wrapper around letterChar
letter :: Parser Char
letter = letterChar

-- | wrapper around digitChar
digit :: Parser Char
digit = digitChar

-- | parses a "decimal representation" (one or more digits [0-9])
decimal :: Parser String
decimal = some digitChar

-- | reserved symbols are either part of the syntax or not allowed in identifiers
reservedSymbols = ['(', ')', '{', '}', '\'', '\"']

-- Lexemes 

-- | an identifier is either a varId or a typeId
identifier :: Parser Atom
identifier = Id <$> (varId <|> typeId)

-- | if it starts with a lowercase char, it is any any sequence of alphanumeric chars,
-- | if it starts with symbol or punctuation, it can only be a sequence of those.
-- | otherwise it's invalid
varId :: Parser Identifier
varId = VarId <$> do
  first <- (lowerChar <|> symbolChar <|> punctuationChar) >>= check
  rest <- (do
              if isSymbol first || isPunctuation first
                then many $ (symbolChar <|> punctuationChar) >>= check
                else many $ alphaNumChar >>= check)
  return $ first:rest
  where check x = if x `elem` reservedSymbols
          then fail $ "char " ++ show x ++ " cannot be in an identifier"
          else return x


-- | any sequence of alphanumeric chars, starting with a uppercase char
typeId :: Parser Identifier
typeId = TypeId <$> ((:) <$> upperChar <*> (many alphaNumChar))

-- | any sequence of digits in decimal representation
intLit :: Parser Literal
intLit = IntLit <$> signed decimal

-- | a decimal, followed by a '.' then another decimal, zero-digits are not allowed
floatLit :: Parser Literal
floatLit = FloatLit <$> signed (show <$> L.float)

-- | reads the given parser, but with an optional sign before it
signed :: Parser String -> Parser String
signed p = do
  sign <- optional (char '+' <|> char '-')
  x <- p
  return $ case sign of
             Nothing -> x
             Just s -> s:x


-- TODO this needs to handle more cases, especially the char escapes
stringLit :: Parser Literal
stringLit = StringLit <$> surroundedBy doubleQuote (many L.charLiteral)
  where doubleQuote = char '"'

charLit :: Parser Literal
charLit = CharLit <$> surroundedBy singleQuote L.charLiteral
  where singleQuote = char '\''


-- | a comment line starts with -- and ends with a '\n', inbetween can be anything
comment :: Parser Comment
comment = Comment <$> between (string "--") (char '\n') inside
  where inside = many (noneOf ['\n'])

-------- Parser --------

-- The toplevel definition of a program
program :: Parser Program
-- the below commented definition only works if Program is to be unified with expSeq
-- program = Program <$> many exprComment
program = Program <$> many ((Left <$> sExp) <|> (Right <$> comment))

sExp :: Parser Sexp
sExp = Sexp <$> between (char '(') (char ')') expSeq

expSeq :: Parser ExpSeq
expSeq = ExpSeq <$> sepBy1 exprComment oneSpace

exprComment :: Parser (Either Exp Comment)         
exprComment = (Left <$> expr) <|> (Right <$> comment)    

expr :: Parser Exp
expr = (ESexp <$> sExp) <|> (EAtom <$> atom) -- TODO <|> infix and indent expressions

atom :: Parser Atom
atom = (Literal <$> literal) <|> identifier

literal :: Parser Literal
literal = (try floatLit) <|> intLit <|> stringLit <|> charLit



