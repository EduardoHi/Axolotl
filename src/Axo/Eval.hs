
module Axo.Eval where

import Axo.AST
import qualified Data.Map as Map
import Data.List
import Control.Monad.State.Strict

evalError msg = error msg



data Value
  = VInt Int
  | VFloat Float
  | VClosure [Name] Expr Env -- arg name, body expr, scope
  | VConstr Name           -- the Constructor function e.g. True, Just  , Left
  | VADT Name [Value]      -- Data Constructor Applied e.g. True, Just 1, Left "error"...  -- "in memory representation", the name of the constructor cannot be erased so that it can be pattern matched
  | VAST Expr              -- "AST" value, to make eval a total function in cases where a value makes no sense, e.g. on a data declaration
  deriving (Show, Eq)

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

extendAll :: Env -> [(String,Value)] -> Env
extendAll env xs = Map.union env (Map.fromList xs)

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
    (Type t) -> evalVar t
    (Defun fname args body) -> -- (define add2 x -> {x + 2})
      evalDefine fname args body
    (If cond expT expF) ->     -- (if condition if-true if-false)
      evalIf cond expT expF
    (Lam arg body) ->          -- (\ x -> (\ y -> (+ x y))) -- Lambda Abstraction
      return $ VClosure arg body env
    (Prim f args) ->
      evalPrim f args
    (App e1 es) ->             -- See Note [Eval Function Application]
      -- TODO: Multiple arguments, i.e. every element of e2, not only the head
      evalApp e1 es
    d@(Data _ constrs) -> do
      evalData constrs
      return $ VAST d
    (Case e cls) ->
      evalCase e cls
    x -> evalError $ "failed to match pattern: " ++ (show x)


evalLit :: Lit -> Evaluator Value
evalLit x = return $ case x of
                       (LitInt i) -> VInt i
                       (LitFloat f) -> VFloat f
                       -- ...

evalData :: [DConstr] -> Evaluator ()
evalData constructors = do
  env <- get
  -- if the constructor is a function type, add it as a VConstr
  -- otherwise, add it as a Value of an ADT
  let names = map (\(name, t) -> if isTArr t
                                 then (name, VConstr name)
                                 else (name, VADT name [])) constructors
  put $ extendAll env names

evalDefine :: String -> [String] -> Expr -> Evaluator Value
evalDefine fname args body = do
  env <- get
  let newc = VClosure args body env
      env' = extend env fname newc
  put $ env'
  return newc

evalCase :: Expr -> [Clause] -> Evaluator Value
evalCase e clauses = do
  caseVal <- eval e
  case caseVal of
    (VADT ne values) ->
      case Data.List.find (\(Clause (CHConstr n) _ _) -> n == ne) clauses of
        Nothing -> error "incomplete patterns in Axo"
        Just (Clause _ args body) -> do
          env <- get
          let bindings = zipWith (,) args values
          put $ extendAll env bindings
          res <- eval body
          put env
          return res
    v -> evalLitClauses v clauses
--    _ -> error $ "expected an ADT, received: " ++ (show caseVal)

evalLitClauses _ [Clause CHAlways args body] = do
  eval body

evalLitClauses val ((Clause (CHLit lit) [] body):clauses) = do
  headval <- eval (Lit lit)
  if headval == val
    then eval body
    else evalLitClauses val clauses


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
    (VADT "True" [])  -> eval eT
    (VADT "False" []) -> eval eF
    _ -> evalError "condition in if is not a bool"

evalApp :: Expr -> [Expr] -> Evaluator Value
evalApp e1 es = do
  env <- get
  objtoapply <- eval e1
  case objtoapply of
    (VClosure params c' env') -> do
      bindings <- (zipWith (,) params) <$> (mapM eval es)
      put $ Map.union env' $ extendAll env $ bindings
      res <- eval c'
      put $ env
      return res
    (VConstr name) -> do
      args <- mapM eval es
      return $ VADT name args
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
  where primitiveFuncs = lookup name (primIntOps ++ primFloatOps ++ compares)


applyVFloat :: (Float -> Float -> Float) -> BinOp
applyVFloat f (VFloat a) (VFloat b) = VFloat (f a b)
applyVFloat _ _ _ = evalError $ "function has wrong type of arguments"

applyVInt :: (Int -> Int -> Int) -> BinOp
applyVInt f (VInt a) (VInt b) = VInt (f a b)
applyVInt _ a b = evalError $ "function has wrong type of arguments: "++(show [a,b])


vTrue =  VADT "True" []
vFalse = VADT "False" []

boolADT b = if b then vTrue else vFalse

eq :: Value -> Value -> Value
eq (VInt a) (VInt b) = boolADT $ a == b

ne :: Value -> Value -> Value
ne (VInt a) (VInt b) = boolADT $ a /= b

compares = [ ("=" , eq)
           , ("!=", ne)
                        ]

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
