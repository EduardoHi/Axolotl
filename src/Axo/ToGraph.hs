{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

 module Axo.ToGraph
   ( toGraph
   , showGraph
   , ToGraph
   , toNode
   ) where

import Control.Monad.State
import Data.Data (Data)

import Axo.ParseTree
import qualified Axo.AST as AST

-- a node in a graph will have:
--   a label (not unique, it's the name of the production rule)
--   an id (the next one in the counter)

-- the connections have an id from and an id to.

-- to print the graph, we print the list of all nodes, and the list of all connections.


-- our state = counter, [Node], [Connections]
-- our value = the resulting graph printed in a string

-- to add a new node:
--   increment counter,
--   add the node to the node list,
--   add all it's connections to the list

-- to calculate the value from the state:
--   print a line for each node with it's id and label
--   print a line for each connection


-- TODO, add siblings correct ordering:
-- https://stackoverflow.com/questions/44274518/how-can-i-control-within-level-node-order-in-graphvizs-dot

type Connection = (Integer, Integer)
type Node = (Integer, String)

type Graph = ([Node], [Connection])
type GraphState = (Integer, Graph)


-- | A fresh id for the node, guaranteed to be different to the rest
fresh :: State GraphState Integer
fresh = do
  (c, (ns, cs)) <- get
  let n = c+1
  put (n, (ns, cs))
  return n

-- newNode adds a new Node to the graph, (increment counter, add to node list)
-- returns only the new id
newNode :: String -> State GraphState Node
newNode label = do
  (_, (ns, cs)) <- get
  i <- fresh
  let newN = (i,label)
  let newG = (newN:ns, cs)
  put (i, newG)
  return newN

-- newConn adss a new Connection to the graph (add Connection to the conn list)
-- returns the new connection
newConn :: Node -> Node -> State GraphState Connection
newConn (i1,_) (i2,_) = do
  (c, (ns, cs)) <- get
  let newC = (i1,i2)
  let newG = (ns, newC:cs)
  put (c, newG)
  return newC


parentOf :: (Data a, ToGraph b) => a -> b -> State GraphState Node
parentOf parent child = parentOf' (newNode (constrName parent)) (toNode child) 

parentOf' :: State GraphState Node -> State GraphState Node -> State GraphState Node
parentOf' parent child = do
  p <- parent
  c <- child
  newConn p c
  return p


childrenOf' children parent = do
  p <- parent
  mapM (\child -> do
           c <- toNode child
           newConn p c) children
  return p  

childrenOf :: (ToGraph a, Data b) => [a] -> b -> State GraphState Node
childrenOf children parent = do
  p <- newNode (constrName parent)
  mapM (\child -> do
           c <- toNode child
           newConn p c) children
  return p

terminal :: String -> State GraphState Node
terminal x = newNode x



class ToGraph a where
  toNode :: a -> State GraphState Node


-- TODO, add a terminal node type, and change this one to that type
instance ToGraph String where
  toNode x = terminal $ escape x
    -- | escape \ char to \\. this is a escaping to make graphviz happy
    where escape = concatMap (\c -> if c == '\\' then "\\\\" else [c])

instance ToGraph Literal where
  toNode (IntLit i) = newNode i
  toNode (FloatLit f) = newNode f
  toNode (StringLit s) = newNode s
  toNode (CharLit c) = newNode $ show c

instance ToGraph Identifier where
  toNode v@(VarId i) = v `parentOf` i
  toNode t@(TypeId i) = t `parentOf` i

instance ToGraph Atom where
  toNode a@(Id i) = a `parentOf` i
  toNode a@(Literal l) = a `parentOf` l

instance ToGraph Sexp where
  toNode s@(Sexp es) = s `parentOf` es

instance ToGraph Exp where
  toNode e@(ESexp s) = e `parentOf` s
  toNode e@(EAtom a) = e `parentOf` a
  toNode e@(EIexp i) = e `parentOf` i
  toNode e@(EInfixexp i) = e `parentOf` i
  toNode e@(EComment c) = e `parentOf` c

instance ToGraph Comment where
  toNode c@(Comment s) = c `parentOf` s

instance ToGraph ExpSeq where
  toNode e@(ExpSeq es) = es `childrenOf` e

instance ToGraph Iexp where
  toNode e@(Iexp head body) = do
    p <- newNode (constrName e)
    parentOf' (return p) (toNode head)
    body `childrenOf'` (return p)
    return p

instance ToGraph InfixExp where
  toNode e@(InfixExp e1 e2 e3) = do
    p <- newNode (constrName e)
    parentOf' (return p) (toNode e1)
    parentOf' (return p) (toNode e2)
    parentOf' (return p) (toNode e3)
    return p

instance ToGraph Program where
  toNode p@(Program es) = es `childrenOf` p
    


-- AST ToGraph instance

instance ToGraph Int where
  toNode i = newNode $ show i

instance ToGraph Float where
  toNode f = newNode $ show f

instance ToGraph Char where
  toNode c = newNode $ show c

instance ToGraph AST.Lit where
  toNode (AST.LitInt i) =  terminal $ "int: " ++ show i
  toNode (AST.LitFloat f) = terminal $ "float: "++ show f
  toNode (AST.LitString s) = terminal $ "string: "++ show s
  toNode (AST.LitChar c) = terminal $ "char: " ++ show c

instance ToGraph AST.Expr where
  toNode (AST.Var v) = toNode v
  toNode (AST.Type t) = toNode t
  toNode (AST.Lit l) = toNode l
  toNode (AST.App func args) = do
    fNode <- toNode func
    args `childrenOf'` (return fNode)
    return fNode
    
instance ToGraph AST.Program where
  toNode p@(AST.Program es) = es `childrenOf` p



--- Desugar ToGraph Instance

instance ToGraph CleanProgram where
  toNode p@(CleanProgram es) = es `childrenOf` p

instance ToGraph CleanExp where
  toNode e@(CleanSexp es) = es `childrenOf` e
  toNode e@(CleanLit  x) = e `parentOf` x
  toNode e@(CleanVar  x) = e `parentOf` x
  toNode e@(CleanType x) = e `parentOf` x

initialState = (0, ([], []))

toGraph :: ToGraph g => g -> Graph
toGraph p = snd $ execState (toNode p) initialState

showGraph g = "digraph graph1 {\n" ++ sg ++ "}"
  where sg = showsGraph g

showsGraph :: Graph -> String
showsGraph (nodes,conns) =
  (foldMap showNode nodes) ++ (foldMap showConns conns)
  where showNode (i,n) =  "    " ++ (show i) ++ " [label= \"" ++ n ++ "\"]; \n"
        showConns (i1, i2) =  "    " ++ (show i1) ++ " -> " ++ (show i2) ++ ";\n"
