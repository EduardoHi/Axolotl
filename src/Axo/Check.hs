module Axo.Check
  ( TypeEnv
  , checkTop
  , checkProgram
  , emptyTypeEnv
  , typeErrorPretty
  ) where

import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.Reader

import Axo.AST


type TypeEnv = Env

type Env = Map.Map String Type

emptyTypeEnv = Map.empty

extendAll :: [(String,Type)] -> Env -> Env
extendAll xs env = Map.union env (Map.fromList xs)

extend :: String -> Type -> Env -> Env
extend name ty env = Map.insert name ty env

eq :: Type -> Type -> Bool
eq TAny _ = True
eq _ TAny = True
eq t1 t2 = t1 == t2

eqs :: [Type] -> [Type] -> Bool
eqs xs ys = and $ zipWith eq xs ys

data TypeError
  = Mismatch [Type] [Type]
  | NotFunction Type
  | UnboundVar String
  deriving (Show)

typeErrorPretty :: TypeError -> String
typeErrorPretty te = "Check.hs " ++ case te of
  Mismatch xs ys -> "Expected: " ++ (show xs) ++ ", but got: " ++ (show ys)
  NotFunction t  -> "Type: " ++ (show t) ++ " is not a function"
  UnboundVar s   -> "Unbound Variable: " ++ s

type Check = ExceptT TypeError (Reader Env)


inEnv :: (Name, Type) -> Check a -> Check a
inEnv (x,t) c = local (extend x t) c

inEnvAll :: [(Name,Type)] -> Check a -> Check a
inEnvAll xts c = local (extendAll xts) c

lookupVar :: String -> Check Type
lookupVar v = do
  env <- ask
  case Map.lookup v env of
    Just e  -> return e
    Nothing -> throwError $ UnboundVar v

defToPairType :: Expr -> (String, Type)
defToPairType (Def s _ t) = case t of
                                Nothing -> (s, TAny)
                                Just t' -> (s, t')

parenScope :: [Expr] -> [(Name, Type)]
parenScope exprs = let defs  = filter isDef  exprs
                       datas = filter isData exprs in
                     (map defToPairType defs) ++ (concatMap getConstrs datas)
  where getConstrs (Data _ cs) = cs


-- the type of an expression list is the last expression
checkExprs :: [Expr] -> Check (Type, Env)
checkExprs xs = do
  let parenEnv = parenScope xs
  tys <- inEnvAll parenEnv (mapM check xs)
  env <- ask
  return (head tys, extendAll parenEnv env)

check :: Expr -> Check Type
check expr = case expr of
  Lit LitInt{}   -> return TInt
  Lit LitFloat{} -> return TFloat
  -- ... LitString ?
  -- ... LitChar   ?
  -- TODO checks for other literals

  Lam names body -> do
    -- this throws and exception when there is no type specified,
    -- change to being inferable later
    let nts = map (\n -> (n,TAny)) names
    rhs <- inEnvAll nts (check body) -- inEnv (name undefined)
    return $ TArr $ (map snd nts)++[rhs]

  Def fname args body ty -> do
    -- if type signature is specified,
    -- we already know the type of the function
    -- then only check that body is correct too.
    -- else type is any.

    case ty of
      Nothing -> return TAny
      Just ty@(TArr tys) -> do
        let argtys = zipWith (,) args (init tys)
        inEnvAll ((fname, ty):argtys) (checkExprs body)
        -- TODO we also need to extend every argument with it's type
        return ty
      Just ty -> do
        inEnv (fname, ty) (checkExprs body)
        -- TODO we also need to extend every argument with it's type
        return ty

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
      (TArr argtypes) | (init argtypes) `eqs` targs -> return $ last argtypes
                      | otherwise -> throwError $ Mismatch (init argtypes) targs
      TAny -> return TAny
      t -> throwError $ NotFunction t

  Prim f args -> do
    let ft = Map.lookup f primEnv
    targs <- mapM check args
    case ft of
      Just (TArr argtypes) | (init argtypes) `eqs` targs -> return $ last argtypes
                           | otherwise -> throwError $ Mismatch (init argtypes) targs
      Just TAny -> return TAny
      Just t -> throwError $ NotFunction t
      Nothing -> throwError $ UnboundVar f

  Var n  -> lookupVar n
  Type t -> lookupVar t

  Data n _ -> return $ TADT n -- tbh this does not even needs to be checked

  x -> error $ "failed to match pattern: " ++ (show x)


runCheck :: Env -> Check a -> Either TypeError a  
runCheck env = flip runReader env . runExceptT

checkTop :: Env -> Expr -> Either TypeError (Type, Env)
checkTop env x = runCheck env $ (checkExprs [x])

checkProgram :: Env -> [Expr] -> Either TypeError (Type, Env)
checkProgram env exps = runCheck env $ (checkExprs exps)

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
