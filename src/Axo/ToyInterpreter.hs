module Axo.ToyInterpreter where

import Control.Monad.State.Strict
import qualified Data.Map as Map


-- this is an unrelated file with the rest of the project, it's only helpful
-- as a test bed of ideas, because it's significantly simpler than
-- the evaluator and the interpreter


emptyEnv :: Env
emptyEnv = Map.empty

type Env = Map.Map String Value

extend :: Env -> String -> Value -> Env
extend env name value = Map.insert name value env



data Expr
  = Lit Int
  | Var String
  | Def String String Expr
  | Let String Expr
  | Add Expr Expr
  | App Expr Expr
  deriving (Show, Eq)
    
data Value
  = VInt Int
  | VClosure String Expr Env
  | VError String
  deriving (Show, Eq)


type Interpreter = State Env

eval :: Expr -> Interpreter Value
eval e = do
  env <- get
  case e of
      (Lit i) -> return $ VInt i
      (Var s) -> return $ maybe (error "var not found") id $ Map.lookup s env
      (Let vname val) -> do
        v <- eval val
        put $ extend env vname v
        return v
      (Def name arg body) -> do
        let c = VClosure arg body env
        put $ extend env name c
        return c
      (Add e1 e2) -> do
        n1 <- eval e1
        n2 <- eval e2
        return $ case (n1,n2) of
          (VInt i1, VInt i2) -> VInt $ i1 + i2
          _ -> VError "Non Int arguments"
      (App e1 e2) -> do
        c <- eval e1
        case c of
          VClosure param c' env' -> do
            v <- eval e2
            put $ extend env' param v
            eval c'
          _ -> return $ VError "not applicable object"


runEval :: [Expr] -> ([Value], Env)
runEval e = runState (mapM eval e) emptyEnv


test1 =
  fst $ runEval [ Let "a" (Lit 1)
                , Def "add2" "x" (Add (Lit 2) (Var "x"))
                , (Add (Lit 2) (Var "a"))
                , (App (Var "add2") (Lit 20))
                ]

runEval2 :: Env -> Expr -> (Value, Env)
runEval2 env e = runState (eval e) env

test2 :: Expr -> Expr -> (Value, Env)
test2 e1 e2 = let (_, env1) = runEval2 emptyEnv e1 in
                runEval2 env1 e2 

