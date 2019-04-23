
module Axo.Eval where

import Axo.AST
import qualified Data.Map as Map
import Control.Monad.State.Strict

type ArgName = String

evalError msg = error msg

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

boundVars :: Env -> [String]
boundVars m = Map.keys m

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
    (Var v) -> evalVar v
    (Type t) -> return $ if t == "True" then (VBool True) else (VBool False) -- UGLY HACK, need to fix how type constructors are handled
    (Def fname arg body _) ->     -- (define add2 x -> {x + 2})
      evalDefine fname arg body
    (If cond expT expF) ->      -- (if condition if-true if-false)
      evalIf cond expT expF
    (Lam arg body _) ->           -- (\ x -> (\ y -> (+ x y))) -- Lambda Abstraction
      return $ VClosure arg body env
    (Prim f args) ->
      evalPrim f args
    (App e1 e2) ->              -- See Note [Eval Function Application]
      -- TODO: Multiple arguments, i.e. every element of e2, not only the head
      evalApp e1 (head e2)
    x -> evalError $ "failed to match pattern: " ++ (show x)


evalLit :: Lit -> Evaluator Value
evalLit x = return $ case x of
                       (LitInt i) -> VInt i
                       (LitFloat f) -> VFloat f
                       -- ...

evalDefine :: String -> String -> [Expr] -> Evaluator Value
evalDefine fname arg body = do
  env <- get
  let newc = VClosure arg (head body) env
  put $ extend env fname newc
  return newc

evalVar :: String -> Evaluator Value
evalVar v = do
  env <- get
  return $ case Map.lookup v env of
             Just v  -> v
             Nothing -> evalError $ "variable "++v++" not found"

evalIf :: Expr -> Expr -> Expr -> Evaluator Value
evalIf cond eT eF = do
  res <- eval cond
  case res of
    (VBool r) -> if r then (eval eT) else (eval eF)
    _ -> evalError "condition in if is not a bool"

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
    _ -> evalError "object not applicable"


evalPrim :: String -> [Expr] -> Evaluator Value
evalPrim name [a,b] = let f = getPrim name in
  do
    a' <- eval a
    b' <- eval b
    return $ f a' b'
evalPrim f args = evalError $ "incorrect arguments: " ++ (show args) ++ ", in function: " ++ f

noFunction name = evalError $ "function "++ name ++ " not found"

getPrim :: String -> BinOp
getPrim name = maybe (noFunction name) id primitiveFuncs
  where primitiveFuncs = lookup name (primIntOps ++ primFloatOps)


applyVFloat :: (Float -> Float -> Float) -> BinOp
applyVFloat f (VFloat a) (VFloat b) = VFloat (f a b)
applyVFloat _ _ _ = evalError $ "function has wrong type of arguments"

applyVInt :: (Int -> Int -> Int) -> BinOp
applyVInt f (VInt a) (VInt b) = VInt (f a b)
appyVInt _ _ _ = evalError $ "function has wrong type of arguments"

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
