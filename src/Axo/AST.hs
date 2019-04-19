{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase            #-}

module Axo.AST where

import Data.Void
import Data.Data
import Data.Proxy()
import qualified Data.List          as DL
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set
import Data.Either

import Text.Megaparsec


import Axo.ParseTree (
  CleanProgram(..),
  CleanExp(..),
  Atom(..),
  Identifier(..),
  Literal(..)
  )

-- the idea of a special form is
-- a set of limited function-like elements that because of their evaluation nature,
-- they need to be implemented directly in the interpreter/compiler
-- and their evaluation/execution is different from normal function application.


data Type
  = TInt        -- e.g. Int
  | TFloat      -- e.g. Float
  | TArr [Type] -- e.g. Int -> Int -> Int
  -- ... user defined types
  deriving (Eq, Read, Show)

type Name = String 

data Lit
  = LitInt Int       -- 10
  | LitFloat Float   -- 1.2
  | LitString String -- "str"
  | LitChar Char     -- 'c'
  deriving (Show, Eq, Data, Ord)

data Expr
  = Var Name
  | Type Name
  | Lit Lit              -- a literal
  | App Expr [Expr]      -- (f a b c) -- apply function f, to arguments a b c
  -- "special forms"
  | Lam Name Expr   -- (\x -> {x + 2})
  | If Expr Expr Expr    -- (if {x > 0} "x is positive" "x is negative")
  | Def Name Name Expr   -- (define f x -> {x + x})
  | Prim Name [Expr]     -- (*. 1.2 3.4)
  deriving (Show, Eq, Data, Ord)

newtype Program = Program [Expr] deriving (Show, Eq, Data)

class ToAST p q where
  toAST :: p -> Either [String] q

instance ToAST CleanProgram Program where
  toAST (CleanProgram exps) = case lefts $ exps' of
                                [] -> Right $ Program $ rights exps'
                                errors -> Left $ concat errors
    where exps' = map toAST exps

instance ToAST CleanExp Expr where
  toAST (CleanSexp cleanExps) =
    case lefts $ expstream of
      [] -> process $ ExprStream $ rights expstream
      errors -> Left $ concat errors
    where expstream = map toAST cleanExps
          process = either (Left . pure . errorBundlePretty) Right . parse pSexp "S-Expression"

  toAST (CleanEAtom atom) =
    Right $ case atom of
      (Id (VarId i)) -> Var i
      (Id (TypeId i)) -> Type i
      (Literal l) -> Lit $ case l of
                             (IntLit i) -> (LitInt (read i))
                             (FloatLit f) -> (LitFloat (read f))
                             (StringLit s) -> LitString s
                             (CharLit c) -> LitChar c

---- utilities

isVar :: Expr -> Bool
isVar Var{} = True
isVar _ = False

primops =
  [ "+" , "-" , "/" , "*"    -- int ops
  , "+.", "-.", "/.", "*."   -- float ops
  ]

---- parsers

pSexp :: Parser Expr
pSexp = pDefinition <|> pIf <|> pLambda <|> pPrim <|> pApp

pExpr :: Parser Expr
pExpr = anySingle

pPrim :: Parser Expr
pPrim = do
  (Var fname) <- oneOf $ map Var primops
  rest <- takeRest
  return $ Prim fname rest

pApp :: Parser Expr
pApp = App <$> pExpr <*> takeRest

pDefinition :: Parser Expr
pDefinition = do
  pVar "define"
  (Var fname) <- pAnyVar
  -- this args parser is not useful yet, because Def currently has only 1 arg right now
  -- args <- takeWhile1P (Just "args") (\x -> (isVar x) && (x /= (Var "->")))
  (Var arg) <- pAnyVar
  pVar "->"
  body <- pExpr
  return $ Def fname arg body

pIf :: Parser Expr
pIf = do
  pVar "if"
  cond <- pExpr
  expT <- pExpr
  expF <- pExpr
  return $ If cond expT expF

pLambda = do
  pVar "\\"
  (Var arg) <- pAnyVar
  body <- pExpr
  return $ Lam arg body

-------------------------------------------------------------
-- Custom Type Parser For Expressions
-------------------------------------------------------------

newtype ExprStream = ExprStream { unTokenStream :: [Expr] }

instance Stream ExprStream where
  type Token  ExprStream = Expr
  type Tokens ExprStream = [Expr]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs

  chunkToTokens Proxy = id

  chunkLength Proxy = length

  chunkEmpty Proxy = null
  take1_ (ExprStream []) = Nothing
  take1_ (ExprStream (t:ts)) = Just (t, ExprStream ts)

  takeN_ n (ExprStream s)
    | n <= 0    = Just ([], ExprStream s)
    | null s    = Nothing
    | otherwise =
        let (x, s') = splitAt n s
        in Just (x, ExprStream s')

  takeWhile_ f (ExprStream s) =
    let (x, s') = DL.span f s
    in (x, ExprStream s')

  showTokens Proxy = DL.intercalate ", "
    . NE.toList
    . fmap show

  reachOffset o pst@PosState {..} =
    let stream = (unTokenStream pstateInput) in
    case drop (o - pstateOffset) stream of
      [] ->
        ( pstateSourcePos
        , show stream
        , pst { pstateInput = ExprStream [] }
        )
      (x:xs) ->
        ( pstateSourcePos
        , show (x:xs)
        , pst { pstateInput = ExprStream (x:xs) }
        )

type Parser = Parsec Void ExprStream

pToken :: Expr -> Parser Expr
pToken c = token test (Set.singleton . Tokens . nes $ c)
  where
    test x = if x == c
             then Just x
             else Nothing
    nes x = x NE.:| []

pVar :: Name -> Parser Expr
pVar v = satisfy (\case
                     (Var n) -> v == n
                     _ -> False)

pAnyVar :: Parser Expr
pAnyVar = satisfy (\case
                      Var {} -> True
                      _ -> False)


