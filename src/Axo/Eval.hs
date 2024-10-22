module Axo.Eval where

import           Axo.AST
import           Control.Monad.State.Strict
import           Data.List
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set

data Value
  = VInt Int
  | VFloat Float
  | VChar Char
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
extendAll env xs = Map.union (Map.fromList xs) env

-- | given a list of names, return an env with only those inside
refineEnv :: [Name] -> Env -> Env
refineEnv ns e = Map.restrictKeys e (Set.fromList ns)
-- order in map union matters, the new env should overwrite the older if there is a
-- shared key

type Evaluator = StateT Env IO

runEval :: Env -> Expr -> IO (Value, Env)
runEval env e = runStateT (eval e) env

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
    (Lam args body) ->         -- (\ x -> (\ y -> (+ x y))) -- Lambda Abstraction
      return $ VClosure args body (refineEnv (freeVars args body) env)
    (Prim f args) ->
      evalPrim f args
    (App e1 es) ->             -- See Note [Eval Function Application]
      evalApp e1 es
    d@(Data _ constrs) -> do
      evalData constrs
      return $ VAST d
    (Case e cls) ->
      evalCase e cls
    (Let a b c) ->
      evalLet a b c
    x -> error $ "failed to match pattern: " ++ (show x)


evalLet :: Name -> Expr -> (Maybe Expr) -> Evaluator Value
evalLet a b c = do
  b' <- eval b
  env <- get
  put $ extend env a b'
  case c of
    Nothing -> return b'
    Just c' -> do
      c'' <- eval c'
      return c''


evalLit :: Lit -> Evaluator Value
evalLit x = return $ case x of
                       (LitInt i)   -> VInt i
                       (LitFloat f) -> VFloat f
                       (LitChar c)  -> VChar c
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
  let newc = VClosure args body (refineEnv (freeVars args body) env)
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
          let extended = extendAll env bindings
          put extended
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
evalLitClauses v _ = error $ "unexpected literal, received: " ++ (show v)

evalVar :: String -> Evaluator Value
evalVar v = do
  env <- get
  return $ case Map.lookup v env of
             Just v  -> v
             Nothing -> error $ "variable "++v++" not found"

evalIf :: Expr -> Expr -> Expr -> Evaluator Value
evalIf cond eT eF = do
  res <- eval cond
  case res of
    (VADT "True" [])  -> eval eT
    (VADT "False" []) -> eval eF
    _                 -> error "condition in if is not a bool"

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
    _ -> error "object not applicable"


evalPrim :: String -> [Expr] -> Evaluator Value
evalPrim name [a,b] = case getPrim name of
  Just f ->
    do
      a' <- eval a
      b' <- eval b
      return $ f a' b'
  Nothing -> noFunction name
evalPrim f args = do
  argvals <- mapM eval args
  getPrimIO f argvals

noFunction name = error $ "function "++ name ++ " not found"

getPrimIO :: String -> [Value] -> Evaluator Value
getPrimIO name = maybe (noFunction name) id $ lookup name primIO

getPrim :: String -> Maybe BinOp
getPrim name = lookup name (primIntOps ++ primFloatOps ++ primComps)


applyVFloat :: (Float -> Float -> Float) -> BinOp
applyVFloat f (VFloat a) (VFloat b) = VFloat (f a b)
applyVFloat _ _ _ = error $ "function has wrong type of arguments"

applyVInt :: (Int -> Int -> Int) -> BinOp
applyVInt f (VInt a) (VInt b) = VInt (f a b)
applyVInt _ a b = error $ "function has wrong type of arguments: "++(show [a,b])


aGetChar :: [Value] -> Evaluator Value
aGetChar [] = VChar <$> liftIO getChar

aPutChar :: [Value] -> Evaluator Value
aPutChar [VChar c] = do
  liftIO $ putChar c
  return vTrue

primIO :: [(String, [Value] -> Evaluator Value)]
primIO = [ ("getChar", aGetChar)
         , ("putChar", aPutChar)
         ]


vLT = VADT "LT" []
vGT = VADT "GT" []
vEQ = VADT "EQ" []

vTrue =  VADT "True" []
vFalse = VADT "False" []

compareVi (VInt a) (VInt b) = compare a b

boolADT b = if b then vTrue else vFalse

eq a b =
  case compareVi a b of
    EQ -> True
    _  -> False

lte a b =
  case compareVi a b of
    GT -> False
    _  -> True

ne a b = not $ eq a b
lt a b = (lte a b) && (not $ eq a b)

gte a b = not $ lt a b
gt a b  = not $ lte a b

peq a b  = boolADT $ eq a b
pne a b  = boolADT $ ne a b

plte a b = boolADT $ lte a b
plt a b  = boolADT $ (lte a b) && (not $ eq a b)

pgte a b = boolADT $ gte a b
pgt a b  = boolADT $ gt a b

primComps = [ ("==" , peq)
            , ("!=" , pne)
            , ("<=" , plte)
            , ("<"  , plt)
            , (">=" , pgte)
            , (">"  , pgt)
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



primEnv = Map.fromList
  [ ("==" , primClosure "==")
  , ("!=" , primClosure "!=")
  , ("<=" , primClosure "<=")
  , ("<"  , primClosure "<")
  , (">=" , primClosure ">=")
  , (">"  , primClosure ">")
  , ("+"  , primClosure "+")
  , ("-"  , primClosure "-")
  , ("/"  , primClosure "/")
  , ("*"  , primClosure "*")
  , ("+." , primClosure "+.")
  , ("-." , primClosure "-.")
  , ("/." , primClosure "/.")
  , ("*." , primClosure "*.")
  ]
  where primClosure name = VClosure ["#x","#y"] (Prim name [Var "#x", Var "#y"]) emptyEnv

