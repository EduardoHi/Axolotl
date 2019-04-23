{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase            #-}

module Axo.AST where

import Data.Data
import Data.Proxy()
import Data.Either

import Text.Megaparsec


import Axo.ExpStream
import Axo.ParseTree (
  CleanProgram(..),
  CleanExp(..),
  Literal(..)
  )

-- the idea of a special form is
-- a set of limited function-like elements that because of their evaluation nature,
-- they need to be implemented directly in the interpreter/compiler
-- and their evaluation/execution is different from normal function application.


data Type
  = TInt        -- e.g. Int
  | TFloat      -- e.g. Float
  | TArr [Type] -- e.g. Int -> Int -> Int
  | TCustom Name -- e.g. List
  -- ... user defined types
  deriving (Eq, Read, Show, Data)

type Name = String 

data Lit
  = LitInt Int       -- 10
  | LitFloat Float   -- 1.2
  | LitString String -- "str"
  | LitChar Char     -- 'c'
  deriving (Show, Eq, Data)

data Expr
  = Var Name
  | Type Name
  | Lit Lit                         -- a literal
  | App Expr [Expr]                 -- (f a b c) -- apply function f, to arguments a b c
  -- "special forms"
  | Lam Name Expr (Maybe Type)      -- (\x -> {x + 2})
  | If Expr Expr Expr               -- (if {x > 0} "x is positive" "x is negative")
  | Def Name [Expr] [Expr] (Maybe Type) -- (define f x -> {x + x})
  | Prim Name [Expr]                -- (*. 1.2 3.4)
  deriving (Show, Eq, Data)

newtype Program = Program [Expr] deriving (Show, Eq, Data)

isDef Def{} = True
isDef _ = False

class ToAST p q where
  toAST :: p -> Either [String] q

instance ToAST CleanProgram Program where
  toAST (CleanProgram exps) = eithers (Right . Program) exps'
    where exps' = map toAST exps

instance ToAST [CleanExp] [Expr] where
  toAST cleanExps = groupEithers $ map toAST cleanExps

instance ToAST CleanExp Expr where
  toAST expr = process pExpr [expr]

-- | eithers, applies function f only if left is empty, otherwise it returns the left in a list
eithers :: ([b] -> Either [a] c) -> [Either [a] b] -> Either [a] c
eithers f xs = case lefts $ xs of
  [] -> f (rights xs)
  errors -> Left $ concat errors


groupEithers :: [Either [a] c] -> Either [a] [c]
groupEithers xs = case lefts $ xs of
                    [] -> Right $ rights xs
                    errors -> Left $ concat errors

---- parsers

pSexp :: Parser Expr
pSexp = pInSexp $ pDefine <|>
        pIf <|>
        pLambda <|>
        pPrim <|>
        pApp

pExpr :: Parser Expr
pExpr = (try pSexp) <|> pAtom

pAtom :: Parser Expr
pAtom = do
  single <- anySingle
  case single of
    CleanSexp _ -> fail $ "Expected an Atom, got: " ++ (show single)
    CleanVar  i -> return $ Var  i
    CleanType t -> return $ Type t
    CleanLit  l -> return $ Lit $ case l of
                                   (IntLit i) -> (LitInt (read i))
                                   (FloatLit f) -> (LitFloat (read f))
                                   (StringLit s) -> LitString s
                                   (CharLit c) -> LitChar c

pPrim :: Parser Expr
pPrim = do
  (CleanVar fname) <- oneOf $ primops
  rest <- some pExpr
  return $ Prim fname rest

primops =  map CleanVar
  [ "+" , "-" , "/" , "*"    -- int ops
  , "+.", "-.", "/.", "*."   -- float ops
  ]

pApp :: Parser Expr
pApp = do
  (x:xs) <- some pExpr
  return $ App x xs

pPatterns :: Parser [Expr]
pPatterns = do
  lhs <- toAST <$> takeWhileP (Just "Pattern") (\case
                                         (CleanVar "->") -> False
                                         (CleanVar _) -> False --- for now, patterns are only valid vars
                                         _ -> True)
  case lhs of
      Left e     -> fail $ concat e
      Right lhs' -> return lhs'

pPattBody :: Parser ([Expr],[Expr])
pPattBody = do
  pInSexp $ do
    pats <- pPatterns
    pArr
    body <- some pExpr
    return (pats, body)

pSimplePattBody :: Parser ([Expr],[Expr])
pSimplePattBody = do
  args <- pPatterns
  pArr
  body <- some pExpr
  return (args, body)

pTypeDecl :: Parser Type
pTypeDecl = undefined

pIf :: Parser Expr
pIf = do
  pVar "if"
  cond <- pExpr
  expT <- pExpr
  expF <- pExpr
  return $ If cond expT expF

pLambda = do
  pVar "\\"
  (CleanVar arg) <- pAnyVar
  typedecl <- optional pTypeDecl
  body <- pExpr
  return $ Lam arg body typedecl

pDefine :: Parser Expr
pDefine = do
  pVar "define"
  (CleanVar fname) <- pAnyVar
  typedecl <- optional pTypeDecl
  (args, body) <- pPattBody <|> pSimplePattBody
  return $ Def fname args body typedecl
