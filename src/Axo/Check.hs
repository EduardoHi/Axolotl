module Axo.Check where

import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.Reader

import Axo.AST



data Type
  = TInt        -- e.g. Int
  | TFloat      -- e.g. Float
  | TArr [Type] -- e.g. Int -> Int -> Int
  -- ...
  deriving (Eq, Read, Show)

type Env = Map.Map String Type

extend :: Env -> String -> Type -> Env

data TypeError
  = Mismatch [Type] [Type]
  | NotFunction Type
  | UnboundVar String
  deriving (Show)

type Check = ExceptT TypeError (Reader Env)





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

  App e args -> do
    t1 <- check e
    targs <- mapM check args
    case t1 of
      (TArr argtypes) | (init argtypes) == targs -> return $ last argtypes
                      | otherwise -> throwError $ Mismatch (init argtypes) targs
      t -> throwError $ NotFunction t

  Var n -> lookupVar n

runCheck :: Env -> Check a -> Either TypeError a  
runCheck env = flip runReader env . runExceptT

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
