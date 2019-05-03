{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase            #-}

module Axo.AST where
{-

This file contains:
1. The definition of important datatypes:
- The Type datatype
- and the Expr datatype, which holds the complete AST before evaluation/codegeneration

-}

import Data.Data

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
                -- `Any` equals `Any` even if they are different, that's absurd so programs are not necessarily correct.
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
