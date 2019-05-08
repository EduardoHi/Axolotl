
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Axo.ToAST where

{-

This file has all the transformation from the desugared Syntax Tree (CleanExp)
to the AST by means of the `ToAST` class and the corresponding `toAST` function.

it uses facilities from Megaparsec to convert join and convert the exp stream

-}

import Data.Either

import Text.Megaparsec

import Axo.PrettyPrinter
import Axo.ExpStream
import Axo.AST
import Axo.ParseTree (
  CleanProgram(..),
  CleanExp(..),
  Literal(..)
  )

class ToAST p q where
  toAST :: p -> Either [String] q

instance ToAST CleanProgram Program where
  toAST (CleanProgram exps) = eithers (Right . Program) exps'
    where exps' = map toAST exps

instance ToAST [CleanExp] [Expr] where
  toAST cleanExps = groupEithers $ map toAST cleanExps

instance ToAST CleanExp Expr where
  toAST expr = process "toAST" pExpr [expr]

-- | eithers, applies function f only iff lefts in the list is empty, otherwise it returns the lefts in the list
eithers :: ([b] -> Either [a] c) -> [Either [a] b] -> Either [a] c
eithers f xs = case lefts $ xs of
  [] -> f (rights xs)
  errors -> Left $ concat errors

-- | groupEithers grabs returns either all the rights grouped in a list, or returns the concatenation of the errors
groupEithers :: [Either [a] c] -> Either [a] [c]
groupEithers xs = case lefts $ xs of
                    [] -> Right $ rights xs
                    errors -> Left $ concat errors

---- parsers

-- | parses sexps forms, i.e. things with the structure of (<something>).
pSexp :: Parser Expr
pSexp = pInSexp $ pDefine <|>
        pIf <|>
        pLambda <|>
        pPrim <|>
        pData <|>
        pList <|>
        pApp

-- | parses either a sexp form, or an atom
pExpr :: Parser Expr
pExpr = (try pSexp) <|> pAtom

-- | parses any of the 3 possible atoms: Var, Type, or Lit. Fails if it meets an sexp
pAtom :: Parser Expr
pAtom = do
  single <- anySingle
  case single of
    CleanSexp _ -> fail $ "AST.hs: Expected an Atom, got: " ++ (show single)
    CleanVar  i -> return $ Var  i
    CleanType t -> return $ Type t
    CleanLit  l -> return $ Lit $ case l of
                                   (IntLit i) -> (LitInt (read i))
                                   (FloatLit f) -> (LitFloat (read f))
                                   (StringLit s) -> LitString s
                                   (CharLit c) -> LitChar c

-- | parses a primitive function
pPrim :: Parser Expr
pPrim = do
  (CleanVar fname) <- oneOf $ primops
  rest <- many pExpr
  return $ Prim fname rest

primops =  map CleanVar
  [ "+" , "-" , "/" , "*"    -- int ops
  , "+.", "-.", "/.", "*."   -- float ops
  , "=", "!="
  , "getChar", "putChar"
  ]

-- | parses a function application
pApp :: Parser Expr
pApp = do
  (x:xs) <- some pExpr
  return $ App x xs

-- | parses the symbol '::', followed by a Type, and a sequence of zero or more ( '->' followed by a Type)
pTypeDecl :: Parser Type
pTypeDecl = do
  pVar "::"
  pInSexp $ do
    types <- pTypeSeq
    return $ typeSeqToType types


typeSeqToType :: [CleanExp] -> Type
typeSeqToType es = case es of
                     [t] -> toType t
                     ts  -> TArr $ map toType ts

-- | converts a CleanExp to Type Data, fails if given CleanExp is not constructed from CleanType
toType :: CleanExp -> Type
toType ce = case ce of
              (CleanType "Int") -> TInt
              (CleanType "Float") -> TFloat
              (CleanType s) -> TADT s
              (CleanVar v) -> TAny -- wish we could have generics instead
              x -> error $ "AST.hs: Expected a CleanType, got: " ++ (show x)

-- | parses the symbol if, followed by 3 expressions, the condition, the expr if true, and the expr if false
pIf :: Parser Expr
pIf = do
  pVar "if"
  If <$> pExpr <*> pExpr <*> pExpr

-- | parses a lambda expression, \, lambda or unicode lambda, an optional type declaration,
-- followed by an argument, an arrow, and then the body
pLambda = do
  oneOf $ map CleanVar ["\\", "lambda", "λ"]
--  (CleanVar arg) <- pAnyVar
  args <- pWhile pAtom  (\x -> x /= (CleanVar "->"))
  pArr
  body <- pExpr
  return $ Lam (map _varName args) body

-- | parses a define expression, the symbol "define", an optional type declaration,
-- followed by a simple pattern-body, or a sequence of pattern bodies.
pDefine :: Parser Expr
pDefine = do
  pVar "define"
  (CleanVar fname) <- pAnyVar
  typedecl <- optional pTypeDecl
  matches <- (singleton <$> try pSinglePattBody) <|> pPattBody
  return $ Def fname matches typedecl
  where singleton x = [x]

-- | parses a list of patterns, used in the left-hand side of a definition
pPatterns :: Parser [Pattern]
pPatterns = pWhile pPattern (\x -> x /= (CleanVar "->"))

-- | parses a single pattern (variable, literal, or a constructor)
pPattern :: Parser Pattern
pPattern = do
  exprToPatt <$> ((try pAtom) <|> (pInSexp pApp))
  where exprToPatt a = case a of
                         Var v          -> PVar v
                         Type t         -> PCon t []
                         Lit l          -> PLit l
                         App (Var  n) l -> PCon n (map exprToPatt l)
                         App (Type n) l -> PCon n (map exprToPatt l)

-- | parses a list of patterns along with an arrow and the body, used in multiple-definitions for functions
pPattBody :: Parser [Equation]
pPattBody = some $ pInSexp pSinglePattBody

-- | parses a simpler pattern with body, that is not inside parenthesis. used in a simpler version of define
pSinglePattBody :: Parser Equation
pSinglePattBody = do
  args <- pPatterns
  pArr
  body <- pExpr
  return $ Equation args body

pConstrDecl :: Name -> Parser DConstr
pConstrDecl tname = do
  pInSexp $ do
    (CleanType cname) <- pAnyType
    args <- typeSeqToType <$> (many (try pAnyType <|> pAnyVar))
    return $ (cname, args `appendTypes` (TADT tname))

pData :: Parser Expr
pData = do
  pVar "data"
  (CleanType tname) <- pAnyType
  constrs <- some (pConstrDecl tname)
  return $ Data tname constrs

pList :: Parser Expr
pList = do
  pVar "["
  exps <- pWhile pExpr (\x -> x /= (CleanVar "]"))
  pVar "]"
  return $ foldr (\e x -> (App (Type "Cons") [e, x])) (Type "Nil") exps
