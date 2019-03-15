module Axo.AST where


type Name = String 

data Lit
  = IntLit Int
  | FloatLit Float
  | String String
  | CharLit Char

data Expr
  = Var Name
  | Type Name
  | Lit 
  | App Expr [Expr]
  deriving (Show, Eq)

newtype Program = Program [Expr] deriving (Show, Eq)

