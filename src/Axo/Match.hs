{-# LANGUAGE LambdaCase #-}
module Axo.Match
  ( matchProgram
  , matchTop
  ) where


import Control.Monad.Reader

import Data.Maybe(fromJust)
import qualified Data.Map as Map
import Data.List(nub, partition)
import Axo.AST


type DataConstr = (Name, Int)

-- DataEnv is a map from a variable name, to it's corresponding type and it's arity
-- e.g. "Nil"  -> ("List", 0)
--      "Cons" -> ("List", 2)
type DataEnv = Map.Map Name DataConstr
type Env = Map.Map Name Type

-- Main match monad, it's a reader to consult:
--    the arity of the constructors
--    the name of the type of a constructor
--    the other constructors from the same type
-- TODO: Match can have errors too, change it to an
-- ExceptT MatchError (Reader DataEnv)
type Match = Reader DataEnv

-- | filter env to only Data Constructors
-- map from (Name, Type), to (Name, Arity)
runMatch :: Env -> Match a -> a
runMatch env = let env' = (mapEnv . filterEnv) env
               in flip runReader env'
  where filterEnv = Map.filter (\case
                                   TArr xs -> isTADT (last xs)
                                   TADT _  -> True
                                   _ -> False)
        mapEnv = Map.map (\case -- not worring because other patterns are filtered beforehand
                             TArr xs -> ((_adtName $ last xs), (length xs) - 1)
                             TADT n  -> (n, 0))

matchProgram :: Env -> Program -> Program
matchProgram env (Program es) = Program $ runMatch env (matchExprs es)

matchTop :: Env -> Expr -> Expr
matchTop env e = runMatch env (matchExpr e)

matchExprs :: [Expr] -> Match [Expr]
matchExprs es = mapM matchExpr es

matchExpr :: Expr -> Match Expr
matchExpr e = case e of
  -- things it does not check:
  -- that the equations are well formed (i.e. have the same number of arguments, or are well typed, or there are only constructors and only literals in corresponding places)
                (Def name eqs _) -> do
                  let k = fromIntegral $ length $ _equationPat (head eqs)
                  let newvars = makeNVar 1 k
                  body <- match k newvars eqs (Var "ERROR")
                  return $ Defun name newvars body
                -- any other expression return it _as it is_
                x -> return x

-- | e.g. "Nil" -> "List"
constrToType :: Name -> Match Name
constrToType name = do
  dataenv <- ask
  return $ fst $ fromJust $ Map.lookup name dataenv

-- | e.g. "List" -> ["Cons", "Nil"]
typeToConstrs :: Name -> Match [Name]
typeToConstrs name = do
  dataenv <- ask
  return $ map fst $ Map.toList $ Map.filter (==name) $ Map.map fst dataenv

-- | Gives the arity of the constructor
-- e.g. (arity (PCon "Nil" [])) -> 0
arity :: Name -> Match Integer
arity name = do
  dataenv <- ask
  return $ fromIntegral $ snd $ fromJust $ Map.lookup name dataenv

-- | returns all the constructors for the type corresponding to the constructor
-- e.g. (constructors "Nil") -> ["Cons", "Nil"]
constructors :: Name -> Match [Name]
constructors = constrToType >=> typeToConstrs

isPVar :: Equation -> Bool
isPVar (Equation (PVar _ : _) _) = True
isPVar Equation{}                = False


isPCon :: Equation -> Bool
isPCon (Equation (PCon _ _ : _) _) = True
isPCon Equation{}                  = False

patLit (PLit l) = l

getPCon :: Equation -> Name
getPCon (Equation (PCon c _ : _) _) = c

getPLit :: Equation -> Lit
getPLit (Equation (PLit l : _) _) = l

makeVar :: Integer -> Name
makeVar k = "_u" ++ (show k)

-- | make n vars starting from k
-- e.g. makeNVar 5 4 => ["_u6","_u7","_u8","_u9"]
makeNVar :: Integer -> Integer -> [Name]
makeNVar k n = take (fromIntegral n) $ map (\i -> makeVar i) [k..]




---- The Pattern Match Compiler Algorithm

-- as described in "The Implementation of Functional Programming Languages" book.

{-

1. Variable Rule

if all patterns start with a variable `v_i`, remove the next `u` and replace inside each body, the variable `v_i` for `u`
match (u:us)             match us
  [ ((v1:ps1), e1),        [ (ps1, e1[u/v1]),
    ...                      ...
    ((vm:psm), em) ]         (psm, em[u/vm]) ]
  E                        E

-}

matchVar :: Integer -> [Name] -> [Equation] -> Expr -> Match Expr
matchVar k (u:us) qs def =
  match k us (map substEq qs) def
  where substEq (Equation ((PVar v):ps) e) = (Equation ps (subst e v u))

{-

2. Constructor Rule

if all patterns start with a constructor, of the same type, say c_1, ..., c_k,
then regroup equations in qs_1, ..., qs_k. then reduce the call of match:

match (u:us) (qs_1, ++ ... +++ qs_k) E

qs_1 = [ ((c_1, ... ]

to:

case u of
   c_1 us'_1 -> match (us'_1 ++ us) qs_1 E
   ...
   c_k us'_k -> match (us'_k ++ us) qs_k E

-}

-- | matchConLit mixes Constructors and Literals
-- they are close to the same, but not close enough to make just one
-- function.
-- this function assumes that literals and Constructors as the first element
-- are never mixed in the same set of equations, and that should hold
-- always. Even if the Checker does not check it. It's safe to assume that
-- an arg being either [] or 0 (for example) is nonsense.
matchConLit :: Integer -> [Name] -> [Equation] -> Expr -> Match Expr
matchConLit k us qs def = do
  if isPCon (head qs)
    then matchCon k us qs def
    else matchLit k us qs def

matchCon k (u:us) qs def = do
  cs <- constructors (getPCon (head qs))
  clauses <- mapM (\c -> matchClause c k (u:us) (choose c qs) def) cs
  return $ Case (Var u) clauses

matchLit k (u:us) qs def = do
  let lits = nub $ map (patLit . head . _equationPat) qs
  clauses <- mapM (\l -> matchLitClause l k (u:us) (chooseLit l qs) def) lits
  return $ Case (Var u) $ clauses++[Clause CHAlways [u] def]

-- | choose returns all equations that begin with constructor c
choose c qs = filter (\q -> getPCon q == c) qs

matchClause :: Name -> Integer -> [Name] -> [Equation] -> Expr -> Match Clause
matchClause c k (_:us) qs def = do
  k' <- arity c
  let us' = makeNVar k k'
  eqs <- match (k' + k) (us'++us) ps'' def
  return $ Clause (CHConstr c) us' eqs
  where
    ps'' = map (\(Equation ((PCon _ ps') : ps) e) -> (Equation (ps'++ps) e)) qs

-- | chooseLit returns all equations that begin with the lit l
chooseLit l qs = filter (\q -> getPLit q == l) qs

matchLitClause :: Lit -> Integer -> [Name] -> [Equation] -> Expr -> Match Clause
matchLitClause l k (_:us) qs def = do
  eqs <- match k us ps'' def
  return $ Clause (CHLit l) [] eqs
  where
    ps'' = map (\(Equation ((PLit _) : ps) e) -> (Equation ps e)) qs


{-

3.  Mixture rule

calling:
match us qs E

where qs has both vars and cons,
we split qs to qs_vars, and qs_cons.
then transform that call to:

match us qs_vars (match us qs_cons E)

4. Empty rule

match []
  [ ( [], e_1),
    ...
    ( [], e_m) ]
  E

is reduced to

e_1 [] .. [] e_m [] E

if we can guarantee that none of e_1, ... e_m can be equal to FAIL,
then it can be further reduced to:

e_1    if m > 0, and
E      if m == 0

-}

-- | match :: A counter -> A variables list -> a list of equations -> default expression -> a lambda case expression
match :: Integer -> [Name] -> [Equation] -> Expr -> Match Expr
-- empty rule
match _ [] qs def =
  do
  return $ if (length qs) > 0
           then head $ (map _equationBody qs)++[def] -- this is the 'optimization'
           else def
-- mixture rule
-- this differs a bit from the book, because partition works different in haskell,
-- but I think it works anyways (constructors and vars are grouped in only 2 groups)
match k us qs def =
  case partition isPVar qs of
    (qsvars, [])     -> matchVar k us qsvars def
    ([], qscons)     -> matchConLit k us qscons def
    (qsvars, qscons) -> do
      m <- matchVar k us qsvars def
      matchConLit k us qscons m
