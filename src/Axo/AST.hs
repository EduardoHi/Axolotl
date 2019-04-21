{-# LANGUAGE FlexibleInstances #-}
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
  | TCustom Name -- e.g. List
  -- ... user defined types
  deriving (Eq, Read, Show, Data, Ord)

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
  | Lit Lit                         -- a literal
  | App Expr [Expr]                 -- (f a b c) -- apply function f, to arguments a b c
  -- "special forms"
  | Lam Name Expr (Maybe Type)      -- (\x -> {x + 2})
  | If Expr Expr Expr               -- (if {x > 0} "x is positive" "x is negative")
  | Def Name Name Expr (Maybe Type) -- (define f x -> {x + x})
  | Prim Name [Expr]                -- (*. 1.2 3.4)
  deriving (Show, Eq, Data, Ord)

newtype Program = Program [Expr] deriving (Show, Eq, Data)

class ToAST p q where
  toAST :: p -> Either [String] q

instance ToAST CleanProgram Program where
  toAST (CleanProgram exps) = eithers (Right . Program) exps'
    where exps' = map toAST exps

instance ToAST [CleanExp] Expr where
  toAST cleanExps = eithers (process pSexp) expstream
    where expstream = map toAST cleanExps

instance ToAST CleanExp Expr where
  toAST atom =
    case atom of
      CleanSexp s -> toAST s
      CleanVar  i -> Right $ Var  i
      CleanType t -> Right $ Type t
      CleanLit  l -> Right $ Lit $ case l of
                                     (IntLit i) -> (LitInt (read i))
                                     (FloatLit f) -> (LitFloat (read f))
                                     (StringLit s) -> LitString s
                                     (CharLit c) -> LitChar c

-- | process is the magical function that applies parser p to a stream of expressions, and returns
-- either a parsing error, or the a new expression
process :: (Parser Expr) -> [Expr] -> Either [String] Expr
process p = either (Left . pure . errorBundlePretty) Right . (parse p "S-Expression" . ExprStream)

-- | eithers, applies function f only if left is empty, otherwise it returns the left in a list
eithers :: ([b] -> Either [a] c) -> [Either [a] b] -> Either [a] c
eithers f xs = case lefts $ xs of
  [] -> f (rights xs)
  errors -> Left $ concat errors
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

primops =
  [ "+" , "-" , "/" , "*"    -- int ops
  , "+.", "-.", "/.", "*."   -- float ops
  ]

pApp :: Parser Expr
pApp = App <$> pExpr <*> takeRest

pTypeDef :: Parser Type
pTypeDef = do
  pVar "::"
  ty  <- pType
  tys <- some $ (pVar "->") *> pType
  return $ if null tys
           then ty
           else TArr (ty:tys)

pPattBody :: Parser (Expr,Expr)
pPattBody = do
  [pat, (Var "->"), body] <- pAppElems
  return (pat, body)

pSimplePattBody :: Parser (Expr,Expr)
pSimplePattBody = do
  arg <- pAnyVar
  pVar "->"
  body <- pExpr
  return (arg,body)

pDefinition :: Parser Expr
pDefinition = do
  pVar "define"
  (Var fname) <- pAnyVar
  typedecl <- optional pTypeDef
  -- this args parser is not useful yet, because Def currently has only 1 arg right now
  -- args <- takeWhile1P (Just "args") (\x -> (isVar x) && (x /= (Var "->")))
  (Var arg,body) <- pPattBody <|> pSimplePattBody
  return $ Def fname arg body typedecl

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
  typedecl <- optional pTypeDef
  body <- pExpr
  return $ Lam arg body typedecl

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

---- utilities

isVar :: Expr -> Bool
isVar Var{} = True
isVar _ = False

isType :: Expr -> Bool
isType Type{} = True
isType _ = False

isApp :: Expr -> Bool
isApp App{} = True
isApp _ = False

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
pAnyVar = satisfy isVar

pAnyType :: Parser Expr
pAnyType = satisfy isType

pAppElems :: Parser [Expr]
pAppElems = do
  (App h t) <- satisfy isApp
  return $ h:t

pType :: Parser Type
pType = do
  ty <- pAnyType
  return $ case ty of
             (Type "Int")   -> TInt
             (Type "Float") -> TFloat
             (Type custom)  -> TCustom custom
