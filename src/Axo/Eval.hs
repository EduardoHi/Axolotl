
module Axo.Eval where

import Axo.AST
import qualified Data.Map as Map
import Control.Monad.State.Strict

type ArgName = String

data Value
  = VInt Int
  | VFloat Float
  | VClosure ArgName Expr Env
  | VBool Bool
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

type Evaluator = State Env

runEval :: Env -> Expr -> (Value, Env)
runEval env e = runState (eval e) env

-- eval :: Env -> Expr -> Value
eval :: Expr -> Evaluator Value
eval term = do
  env <- get
  case term of
    (Lit l) -> evalLit l
    (Var v) -> evalVar env v
    (Type t) -> return $ if t == "True" then (VBool True) else (VBool False) -- UGLY HACK should evaluate then check, it needs boolean primitives first (eq, and, or, not)
    (App (Var "define") rest) ->
      evalDefine env rest
    (App (Var "if") rest) -> -- (if condition if-true if-false)
      evalIf rest
    (App (Var "\\") rest) -> -- (\ x -> (\ y -> (+ x y))) -- Lambda Abstraction
      evalLambda env rest
    (App (Var f) (a:b:[])) -> -- See Note [Eval Function Application]
      do
        av <- eval a
        bv <- eval b
        return $ evalPrim f av bv
    (App e1 e2) ->
      -- TODO: Multiple arguments, i.e. every element of e2, not only the head
      evalApp e1 (head e2)
    x -> error $ "failed to match pattern: "++(show x)

  where evalLit x = return $ case x of
                             (LitInt i) -> VInt i
                             (LitFloat f) -> VFloat f
                             -- ...

splitWhere :: (a -> Bool) -> [a] -> ([a],[a])
splitWhere f ls = splitWhere' ls []
  where splitWhere' [] _ = ([],[])
        splitWhere' (x:xs) pre
          | f x       = (reverse pre,xs)
          | otherwise = splitWhere' xs (x:pre)

evalDefine :: Env -> [Expr] -> Evaluator Value
evalDefine env rest =
  let (Var fname):rest' = rest
      splitted = splitWhere (== Var "->") rest' in
      case splitted of
        ([Var arg], [body]) -> do
          let newc = VClosure arg body env
          put $ extend env fname newc
          return newc
        (_,_) -> error $ "Malformed Define: "++(show splitted)

evalLambda :: Env -> [Expr] -> Evaluator Value
evalLambda env rest =
  return $ case splitWhere (== Var "->") rest of
             ([Var arg], [body]) -> VClosure arg body env
             (_,_) -> error $ "Malformed Lambda: "++(show rest)

evalVar :: Env -> String -> Evaluator Value
evalVar env v =
  return $ case Map.lookup v env of
             Just v  -> v
             Nothing -> error $ "variable "++v++" not found"

evalIf :: [Expr] -> Evaluator Value
evalIf (cond:eT:eF:[]) = do
  res <- eval cond
  case res of
    (VBool r) -> if r then (eval eT) else (eval eF)
    _ -> error "condition in if is not a bool"

evalApp :: Expr -> Expr -> Evaluator Value
evalApp e1 e2 = do
  env <- get
  closure <- eval e1
  case closure of
    (VClosure param c' env') -> do
      v <- eval e2
      put $ extend env' param v
      res <- eval c'
      put $ env
      return res
    _ -> error "object not applicable"
  
  -- let VClosure an c env' = eval env e1 in
  --   let v = eval env e2 in
  --     eval (extend env' an v) c


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
