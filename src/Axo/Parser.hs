{-# LANGUAGE TupleSections #-}
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
data Program = Program [Either Exp Comment] deriving (Show, Eq)

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

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) empty empty
  where f ch = ch == ' '

scn = L.space space1 empty empty  


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
  first <- (lowerChar <|> validSymbol)
  rest <- (do
              if isSymbol first || isPunctuation first
                then syms -- many $ (symbolChar <|> punctuationChar) >>= check
                else many alphaNumChar)
  return $ first:rest
  where syms = many validSymbol
        validSymbol = satisfy (\x -> (isSymbol x || isPunctuation x) && not (x `elem` reservedSymbols))


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

stringLit :: Parser Literal
stringLit = StringLit <$> (doubleQuote >> manyTill L.charLiteral doubleQuote)
   where doubleQuote = char '"'

charLit :: Parser Literal
charLit = CharLit <$> surroundedBy singleQuote L.charLiteral
  where singleQuote = char '\''

-- | a comment line starts with -- and ends with a '\n', inbetween can be anything
comment :: Parser Comment
comment = lexeme $ (Comment <$> between (string "--") (char '\n') inside <?> "Comment")
  where inside = many $ noneOf ['\n']

-------- Parser --------

-- The toplevel definition of a program
program :: Parser Program
program = Program <$> (many $ L.nonIndented scn topLevelDecl)
  where topLevelDecl = (Right <$> comment) <|> (Left <$> topLevelExps)
        topLevelExps = (EIexp <$> indentExp) <|>
                       (EInfixexp <$> infixExp) <|>
                       (ESexp <$> sExp)
                       

sExp :: Parser Sexp
sExp = Sexp <$> between (char '(') (char ')') sexpseq <?> "Sexpression"
  where sexpseq = ExpSeq <$> (L.lineFold scn $ \sp -> x `sepBy1` try sp <* scn)
        x = (Right <$> comment) <|> (Left <$> expr)

expSeq :: Parser ExpSeq
expSeq = ExpSeq <$> many exprComment

exprComment :: Parser (Either Exp Comment)         
exprComment = lexeme $ (Right <$> comment) <|> (Left <$> expr)

expr :: Parser Exp
expr = lexeme $ (ESexp <$> sExp)
       <|> (EInfixexp <$> infixExp)
       <|> (EAtom <$> atom)
       -- <|> TODO: Nested indentExpressions <|> (EIexp <$> indentExp)

-- for now, infix expressions are only 3 expressions
infixExp :: Parser InfixExp
infixExp = (do 
  char '{'
  e1 <- expr
  e2 <- expr
  e3 <- expr
  char '}'
  return $ InfixExp e1 e2 e3) <?> "InfixExpression"

indentExp :: Parser Iexp
indentExp = (uncurry Iexp) <$> L.indentBlock space p
  where
    p = do
      header <- iexpseq -- (:) <$> (Left . EAtom <$> identifier) <*> expSeq
      return (L.IndentSome Nothing (return . (header, )) expSeq)
    iexpseq = ExpSeq <$> ( (:) <$> (Left . EAtom <$> (lexeme identifier)) <*> (many exprComment))

-- indentItem =       

iexp = L.indentBlock space p
  where
    p = do
      header <- atom
      return (L.IndentSome Nothing (return . (header, )) atom)

atom :: Parser Atom
atom = (try $ Literal <$> literal) <|> identifier <?> "Atom"

literal :: Parser Literal
literal = (try floatLit) <|> intLit <|> stringLit <|> charLit <?> "Literal"


parseProgram :: String -> String
parseProgram input = case parse program "Axolotl" input of
  Left err -> "error" ++ (errorBundlePretty err)
  Right val -> show val
  
