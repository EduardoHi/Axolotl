module Axo.Check
  ( TypeEnv
  , checkTop
  , emptyTypeEnv
  , typeErrorPretty
  ) where

import qualified Data.Map as Map
import Data.Maybe(fromJust)

import Control.Monad.Except
import Control.Monad.Reader

import Axo.AST

type TypeEnv = Env

type Env = Map.Map String Type

emptyTypeEnv = Map.empty

extend :: String -> Type -> Env -> Env
extend name ty env = Map.insert name ty env

data TypeError
  = Mismatch [Type] [Type]
  | NotFunction Type
  | UnboundVar String
  deriving (Show)

typeErrorPretty :: TypeError -> String
typeErrorPretty te = case te of
  Mismatch xs ys -> "Expected: " ++ (show xs) ++ ", but got: " ++ (show ys)
  NotFunction t  -> "Type: " ++ (show t) ++ " is not a function"
  UnboundVar s   -> "Unbound Variable: " ++ s

type Check = ExceptT TypeError (Reader Env)


inEnv :: (Name, Type) -> Check a -> Check a
inEnv (x,t) c = do
  local (extend x t) c


lookupVar :: String -> Check Type
lookupVar v = do
  env <- ask
  case Map.lookup v env of
    Just e  -> return e
    Nothing -> throwError $ UnboundVar v

check :: Expr -> Check Type
check expr = case expr of
  Lit LitInt{}   -> return TInt
  Lit LitFloat{} -> return TFloat
  -- ... LitString ?
  -- ... LitChar   ?
  -- TODO checks for other literals

  Lam name body ty -> do
    -- this throws and exception when there is no type specified,
    -- change to being inferable later
    rhs <- inEnv (name,fromJust ty) (check body) -- inEnv (name undefined)
    return (TArr [fromJust ty,rhs])

  Def fname arg body ty -> do
    -- if type signature is specified,
    -- we already know the type of the function
    -- then only check that body is correct too.
    -- else infer type.

    -- we need to extend all the current scope of the function with this function's name
    inEnv (arg, fromJust ty) (check body)
    return (fromJust ty)

  If cond eT eF -> do
    -- cond should be a boolean
           -- condt <- check cond
           -- when (condt /= TBool) (throwError $ Mismatch TBool condt)
    -- eT's type should be equal to eF's type
    eTt <- check eT
    eFt <- check eF
    when (eTt /= eFt) (throwError $ Mismatch [eTt] [eFt])
    -- if both are equal, it does not matter which we return
    return eTt
  
  App e args -> do
    t1 <- check e
    targs <- mapM check args
    case t1 of
      (TArr argtypes) | (init argtypes) == targs -> return $ last argtypes
                      | otherwise -> throwError $ Mismatch (init argtypes) targs
      t -> throwError $ NotFunction t

  Prim f args -> do
    let ft = Map.lookup f primEnv
    targs <- mapM check args
    case ft of
      Just (TArr argtypes) | (init argtypes) == targs -> return $ last argtypes
                           | otherwise -> throwError $ Mismatch (init argtypes) targs
      Just t -> throwError $ NotFunction t
      Nothing -> throwError $ UnboundVar f

  Var n -> lookupVar n

runCheck :: Env -> Check a -> Either TypeError a  
runCheck env = flip runReader env . runExceptT

checkTop :: Env -> Expr -> Either TypeError Type
checkTop env x = runCheck env $ (check x)

checkWithPrim expr = runCheck primEnv (check expr)

primEnv = Map.fromList [
                       -- int ops
                         ("+" , TArr [TInt, TInt, TInt])
                       , ("-" , TArr [TInt, TInt, TInt])
                       , ("/" , TArr [TInt, TInt, TInt])
                       , ("*" , TArr [TInt, TInt, TInt])
                       -- float ops
                       , ("+.", TArr [TFloat, TFloat, TFloat])
                       , ("-.", TArr [TFloat, TFloat, TFloat])
                       , ("/.", TArr [TFloat, TFloat, TFloat])
                       , ("*.", TArr [TFloat, TFloat, TFloat])
                       ]
