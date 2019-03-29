
module Eval where

import Axo.AST

data Value
  = VInt Int
  | VFloat Float
  deriving Show



type Env = [Value]

-- eval :: Env -> Expr -> Value
eval :: Expr -> Value
eval term = case term of
  (Lit l) -> evalLit l
  -- ...
  (App (Var f) (a:b:_)) ->
    -- things this does not handle right now (maybe it should not handle it at all, i.e. this only evaluates correct programs):
    -- if f is not a primitive function,
    -- if there is an incorrect number of args
    -- if the first expr is not a function at all
    evalPrim f (eval a) (eval b)

  where evalLit x = case x of
          (LitInt i) -> VInt i
          (LitFloat f) -> VFloat f
          -- ...

-- TODO: Proper exception handling in the interpreter
evalPrim name a b = case lookup name primIntOps of
                      Just f -> f a b
                      Nothing -> (case lookup name primFloatOps of
                                    Just f -> f a b
                                    Nothing -> error $ "function "++ name ++ " not found")


applyVFloat :: (Float -> Float -> Float) -> Value -> Value -> Value
applyVFloat f (VFloat a) (VFloat b) = VFloat (f a b)
applyVFloat _ _ _ = error $ "function has wrong type of arguments"

applyVInt :: (Int -> Int -> Int) -> Value -> Value -> Value
applyVInt f (VInt a) (VInt b) = VInt (f a b)
appyVInt _ _ _ = error $ "function has wrong type of arguments"

primIntOps = [ ("+", applyVInt (+) )
             , ("-", applyVInt (-) )
             , ("/", applyVInt div )
             , ("*", applyVInt (*) )
             ]

primFloatOps = [ ("+.", applyVFloat (+) )
               , ("-.", applyVFloat (-) )
               , ("/.", applyVFloat (/) )
               , ("*.", applyVFloat (*) )
               ]
