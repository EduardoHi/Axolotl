{-# LANGUAGE MultiParamTypeClasses #-}

module Axo.AST where


import Axo.ParseTree (
  CleanProgram(..),
  CleanExp(..),
  Atom(..),
  Identifier(..),
  Literal(..)
  )


type Name = String 

data Lit
  = LitInt Int
  | LitFloat Float
  | LitString String
  | LitChar Char
  deriving (Show, Eq)

data Expr
  = Var Name
  | Type Name
  | Lit Lit
  | App Expr [Expr]
  deriving (Show, Eq)

newtype Program = Program [Expr] deriving (Show, Eq)


class ToAST p q where
  toAST :: p -> q


instance ToAST CleanProgram Program where
  toAST (CleanProgram exps) = Program $ map toAST exps

instance ToAST CleanExp Expr where
  toAST (CleanSexp cleanExps) = App (head exps) (tail exps)
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

