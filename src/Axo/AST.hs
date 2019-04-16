{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Axo.AST where

import Data.Data

import Axo.ParseTree (
  CleanProgram(..),
  CleanExp(..),
  Atom(..),
  Identifier(..),
  Literal(..)
  )

-- the idea of a special form is
-- a set of limited function-like elements that because of their evaluation nature,
-- they need to be implemented directly in the interpreter/compiler
-- and their evaluation/execution is different from normal function application.

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
  | Lit Lit            -- a literal
  | App Expr [Expr]    -- (f a b c) -- apply function f, to arguments a b c
  -- "special forms"
  | Lam Name Expr      -- (\x -> {x + 2})
  | If Expr Expr Expr  -- (if {x > 0} "x is positive" "x is negative")
  | Def Name Name Expr -- (define f x -> {x + x})
  | Prim Expr [Expr]     -- (*. 1.2 3.4)
  deriving (Show, Eq, Data)



newtype Program = Program [Expr] deriving (Show, Eq, Data)


class ToAST p q where
  toAST :: p -> q


instance ToAST CleanProgram Program where
  toAST (CleanProgram exps) = Program $ map toAST exps

instance ToAST CleanExp Expr where
  toAST (CleanSexp cleanExps) =
    case exps of
      (Var "define"):rest -> defineToAst rest
      (Var "if"):rest     -> ifToAst rest
      (Var "\\"):rest     -> lambdaToAst rest
      h@(Var fun):rest    -> if fun `elem` primops
        then Prim h rest
        else App h rest
      _                   -> App (head exps) (tail exps)
    where exps = map toAST cleanExps

  toAST (CleanEAtom atom) =
    case atom of
      (Id (VarId i)) -> Var i
      (Id (TypeId i)) -> Type i
      (Literal l) -> Lit $ case l of
                             (IntLit i) -> (LitInt (read i))
                             (FloatLit f) -> (LitFloat (read f))
                             (StringLit s) -> LitString s
                             (CharLit c) -> LitChar c


primops =
  [ "+" , "-" , "/" , "*"    -- int ops
  , "+.", "-.", "/.", "*."   -- float ops
  ]

lambdaToAst rest =
  case splitWhere (== Var "->") rest of
             ([Var arg], [body]) -> Lam arg body
             (_,_) -> error $ "Malformed Lambda: "++(show rest)

defineToAst rest =
  let (Var fname):rest' = rest
      splitted = splitWhere (== Var "->") rest' in
    case splitted of
      ([Var arg], [body]) -> Def fname arg body
      _ -> error $ "Malformed Define: " ++ (show splitted)

ifToAst [cond, expT, expF] =
 If cond expT expF
ifToAst rest = error $ "Malformed If: " ++ (show rest)



splitWhere :: (a -> Bool) -> [a] -> ([a],[a])
splitWhere f ls = splitWhere' ls []
  where splitWhere' [] _ = ([],[])
        splitWhere' (x:xs) pre
          | f x       = (reverse pre,xs)
          | otherwise = splitWhere' xs (x:pre)
