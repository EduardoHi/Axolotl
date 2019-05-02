{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase            #-}

module Axo.AST where
{-

This file contains 2 important things:
1. The definition of important datatypes:
- The Type datatype
- and the Expr datatype, which holds the complete AST before evaluation/codegeneration

2. The transformation from the desugared Syntax Tree (CleanExp) to the AST
by means of the `ToAST` class and the corresponding `toAST` function.

-}

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

-- Note [Special Forms]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- the idea of a special form is
-- a set of limited function-like elements that because of their evaluation nature,
-- they need to be implemented directly in the interpreter/compiler
-- and their evaluation/execution is different from normal function application.


data Type
  = TInt        -- e.g. Int
  | TFloat      -- e.g. Float
  | TArr [Type] -- e.g. Int -> Int -> Int
  | TADT Name   -- e.g. List
  | TAny        -- infamous and hacky Any type, but needed, because of lack of inference :(
                -- things without a type signature are marked as any and never fail to check
                -- Any equals Any even if they are different. so that's absurd.
  deriving (Eq, Read, Show, Data)

isTArr TArr{} = True
isTArr _      = False

appendTypes :: Type -> Type -> Type
appendTypes (TArr []) t = t
appendTypes t (TArr []) = t
appendTypes (TArr ta) (TArr tb) = TArr (ta ++ tb)
appendTypes (TArr ta) t = TArr (ta ++ [t])
appendTypes t (TArr ta) = TArr (t:ta)
appendTypes tx ty = TArr [tx,ty]

type Name = String 

data Lit
  = LitInt Int       -- 10
  | LitFloat Float   -- 1.2
  | LitString String -- "str"
  | LitChar Char     -- 'c'
  deriving (Show, Eq, Data)

type Constrs = (Name,Type)

data Expr
  = Var Name                            -- abc       -- a symbol
  | Type Name                           -- List      -- a capitalized symbol
  | Lit Lit                             -- a literal
  | App Expr [Expr]                     -- (f a b c) -- apply function f, to arguments a b c
                                        -- "special forms" -- see Note [Special Forms]
  | Lam [Name] Expr                     -- (\x -> {x + 2})
  | If Expr Expr Expr                   -- (if {x > 0} "x is positive" "x is negative")
  | Def Name [Name] [Expr] (Maybe Type) -- (define f x -> {x + x})
  | Prim Name [Expr]                    -- (*. 1.2 3.1)
  | Data Name [Constrs]                 -- (data Bool (True) (False))
  deriving (Show, Eq, Data)

newtype Program = Program [Expr] deriving (Show, Eq, Data)

isDef Def{}   = True
isDef _       = False

isData Data{} = True
isData _      = False

isVar Var{}   = True
isVar _       = False

_varName (Var n) = n

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
  rest <- some pExpr
  return $ Prim fname rest

primops =  map CleanVar
  [ "+" , "-" , "/" , "*"    -- int ops
  , "+.", "-.", "/.", "*."   -- float ops
  ]

-- | parses a function application
pApp :: Parser Expr
pApp = do
  (x:xs) <- some pExpr
  return $ App x xs

-- | parses a list of patterns, used in the left-hand side of a definition
pPatterns :: Parser [Expr]
pPatterns = do
  lhs <- toAST <$> takeWhileP (Just "Pattern") (\case
                                         (CleanVar "->") -> False
                                         (CleanVar _) -> True --- for now, patterns are only valid vars
                                         _ -> False)
  case lhs of
      Left e     -> fail $ concat e
      Right lhs' -> return lhs'

-- | parses a list of patterns along with an arrow and the body, used in multiple-definitions for functions
pPattBody :: Parser ([Expr],[Expr])
pPattBody = pInSexp pSimplePattBody

-- | parses a simpler pattern with body, that is not inside parenthesis. used in a simpler version of define
pSimplePattBody :: Parser ([Expr],[Expr])
pSimplePattBody = do
  args <- pPatterns
  pArr
  body <- some pExpr
  return (args, body)

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
  args <- pPatterns
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
  (args, body) <- (try pSimplePattBody) <|> pPattBody
  return $ Def fname (map _varName args) body typedecl

pConstrDecl :: Name -> Parser Constrs
pConstrDecl tname = do
  pInSexp $ do
    (CleanType cname) <- pAnyType
    args <- typeSeqToType <$> (many pAnyType)
    return $ (cname, args `appendTypes` (TADT tname))

pData :: Parser Expr
pData = do
  pVar "data"
  (CleanType tname) <- pAnyType
  constrs <- some (pConstrDecl tname)
  return $ Data tname constrs
