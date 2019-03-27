{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Axo.Parser where

import Control.Monad
import Control.Applicative hiding (many, some)

import Data.Void
import Data.Char
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Axo.ParseTree


type Parser = Parsec Void String
type ParseError = ParseErrorBundle String Void

-------- Lexer --------

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) empty empty
  where f ch = ch == ' '

scn :: Parser ()
scn = L.space space1 empty empty  


-- | equivalent to `between by by`
surroundedBy :: Parser b -> Parser a -> Parser a
surroundedBy by = between by by

lexeme :: Parser a -> Parser a
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
reservedSymbols :: [Char]
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
program = Program <$> (many $ L.nonIndented scn topLevelExps)
  where topLevelExps = (EIexp <$> indentExp) <|>
                       (EInfixexp <$> infixExp) <|>
                       (ESexp <$> sExp) <|>
                       (EComment <$> comment)
                       

sExp :: Parser Sexp
sExp = Sexp <$> between (char '(') (char ')') sexpseq <?> "Sexpression"
  where sexpseq = ExpSeq <$> (L.lineFold scn $ \sp -> expr `sepBy1` (try sp <|> scn))

expSeq :: Parser ExpSeq
expSeq = ExpSeq <$> many exprComment

expr :: Parser Exp
expr = lexeme $ (EComment <$> try comment)
       <|> (ESexp <$> sExp)
       <|> (EInfixexp <$> infixExp)
       <|> (EAtom <$> atom)
       -- TODO: Nested indentExpressions <|> (EIexp <$> indentExp)

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
    iexpseq = ExpSeq <$> ( (:) <$> (EAtom <$> (lexeme identifier)) <*> (many expr))


atom :: Parser Atom
atom = (try $ Literal <$> literal) <|> identifier <?> "Atom"

literal :: Parser Literal
literal = (try floatLit) <|> intLit <|> stringLit <|> charLit <?> "Literal"



parseProgram :: String -> Either ParseError Program
parseProgram input = parse program "Axolotl" input


-- | tihs function runs the parser, but also recieve a function f
-- | that indicates what to do with the result of parsing `Program`
runParser :: String -> (Program -> String) -> String
runParser s f =
  case parseProgram s of
    Left err -> "Error: " ++ (errorBundlePretty err)
    Right val -> f val
