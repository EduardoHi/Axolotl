
module Axo.Eval where

import Axo.AST
import qualified Data.Map as Map

type ArgName = String

data Value
  = VInt Int
  | VFloat Float
  | VClosure ArgName Expr Env
  deriving Show

-- Note [Eval Function Application]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- things that are not handled (maybe it should not handle it at all, i.e. this only evaluates correct programs):
-- if f is not a primitive function,
-- if there is an incorrect number of args
-- if the first expr is not a function at all

emptyEnv :: Env
emptyEnv = Map.empty

type Env = Map.Map String Value
type BinOp = Value -> Value -> Value

extend :: Env -> String -> Value -> Env
extend env name value = Map.insert name value env

-- eval :: Env -> Expr -> Value
eval :: Env -> Expr -> Value
eval env term = case term of
  (Lit l) -> evalLit l
  (Var v) -> evalVar env v
  (App (Var "\\") rest) -> -- (\ x -> (\ y -> (+ x y))) -- Lambda Abstraction
    evalLambda env rest
  (App (Var f) (a:b:[])) -> -- See Note [Eval Function Application]
    evalPrim f (eval env a) (eval env b)
  (App e1 e2) ->
    -- TODO: Multiple arguments, i.e. every element of e2, not only the head
    evalApp env e1 (head e2)
  x -> error $ "failed to match pattern: "++(show x)

  where evalLit x = case x of
          (LitInt i) -> VInt i
          (LitFloat f) -> VFloat f
          -- ...

splitWhere :: (a -> Bool) -> [a] -> ([a],[a])
splitWhere f ls = splitWhere' ls []
  where splitWhere' [] _ = ([],[])
        splitWhere' (x:xs) pre
          | f x       = (reverse pre,xs)
          | otherwise = splitWhere' xs (x:pre)

evalLambda env rest =
  case splitWhere (== Var "->") rest of
    ([Var arg], [body]) -> VClosure arg body env
    (_,_) -> error $ "Malformed Lambda: "++(show rest)

evalVar :: Env -> String -> Value
evalVar env v =
  case Map.lookup v env of
    Just v  -> v
    Nothing -> error $ "variable "++v++" not found"

evalApp env e1 e2 =
  let VClosure an c env' = eval env e1 in
    let v = eval env e2 in
      eval (extend env' an v) c

evalPrim :: String -> BinOp
evalPrim name a b = (getPrim name) a b

noFunction name = error $ "function "++ name ++ " not found"

getPrim :: String -> BinOp
getPrim name = maybe (noFunction name) id primitiveFuncs
  where primitiveFuncs = lookup name (primIntOps ++ primFloatOps)


applyVFloat :: (Float -> Float -> Float) -> BinOp
applyVFloat f (VFloat a) (VFloat b) = VFloat (f a b)
applyVFloat _ _ _ = error $ "function has wrong type of arguments"

applyVInt :: (Int -> Int -> Int) -> BinOp
applyVInt f (VInt a) (VInt b) = VInt (f a b)
appyVInt _ _ _ = error $ "function has wrong type of arguments"

primIntOps :: [(String, BinOp)]
primIntOps = [ ("+", applyVInt (+) )
             , ("-", applyVInt (-) )
             , ("/", applyVInt div )
             , ("*", applyVInt (*) )
             ]

primFloatOps :: [(String, BinOp)]
primFloatOps = [ ("+.", applyVFloat (+) )
               , ("-.", applyVFloat (-) )
               , ("/.", applyVFloat (/) )
               , ("*.", applyVFloat (*) )
               ]
