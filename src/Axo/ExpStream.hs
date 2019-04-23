{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Axo.ExpStream where

{-

This file declares the custom parser using CleanExp as a token type,
and [CleanExp] as the stream type.

it also exports useful "low level" parsers or "lexers" to be used in other
files.

-}

import Data.Void
import Data.Data
import Data.Proxy()
import qualified Data.List          as DL
import qualified Data.List.NonEmpty as NE

import Axo.ParseTree(CleanExp(..))

import Text.Megaparsec




newtype ExpStream = ExpStream { unExpStream :: [CleanExp] }

instance Stream ExpStream where
  type Token  ExpStream = CleanExp
  type Tokens ExpStream = [CleanExp]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs

  chunkToTokens Proxy = id

  chunkLength Proxy = length

  chunkEmpty Proxy = null
  take1_ (ExpStream []) = Nothing
  take1_ (ExpStream (t:ts)) = Just (t, ExpStream ts)

  takeN_ n (ExpStream s)
    | n <= 0    = Just ([], ExpStream s)
    | null s    = Nothing
    | otherwise =
        let (x, s') = splitAt n s
        in Just (x, ExpStream s')

  takeWhile_ f (ExpStream s) =
    let (x, s') = DL.span f s
    in (x, ExpStream s')

  showTokens Proxy = DL.intercalate ", "
    . NE.toList
    . fmap show

  reachOffset o pst@PosState {..} =
    let stream = (unExpStream pstateInput) in
    case drop (o - pstateOffset) stream of
      [] ->
        ( pstateSourcePos
        , show stream
        , pst { pstateInput = ExpStream [] }
        )
      (x:xs) ->
        ( pstateSourcePos
        , show (x:xs)
        , pst { pstateInput = ExpStream (x:xs) }
        )

type Parser = Parsec Void ExpStream

-- | process is the magical function that applies parser p to a stream of expressions, and returns
-- either a parsing error, or the a new expression
process :: String -> (Parser a) -> [CleanExp] -> Either [String] a
process parsername p = either (Left . pure . errorBundlePretty) Right . (parse p parsername . ExpStream)

-- | runs the parser p inside of a sexp. fails if it's not a sexp, or if the parser p fails.
pInSexp :: Parser a -> Parser a
pInSexp p = do
  next <- anySingle
  case next of
    (CleanSexp exps) -> case process "inside s-exp" p exps of
                          Left errors -> fail $ concat errors
                          Right exp -> return exp
    _                -> fail $ "Expected a CleanSexp, got " ++ (show next)

-- | parses the symbol '::', followed by a Type, and a sequence of zero or more ( '->' followed by a Type)
pTypeSeq :: Parser [CleanExp]
pTypeSeq = do
  pVar "::"
  pInSexp $ do
    ty  <- pAnyType
    tys <- (some $ pArr *> (pAnyType))
    return $ (ty:tys)

--- "Lexer" 

pAnyVar :: Parser CleanExp
pAnyVar  = satisfy isVar  <?> "CleanVar"

pAnyType :: Parser CleanExp
pAnyType = satisfy isType <?> "CleanType"

pAnyLit :: Parser CleanExp
pAnyLit  = satisfy isLit  <?> "CleanLit"

pAnySexp :: Parser CleanExp
pAnySexp = satisfy isSexp <?> "CleanSexp"

pVar :: String -> Parser CleanExp
pVar v = satisfy (\case
                     (CleanVar n) -> v == n
                     _ -> False)

pType :: String -> Parser CleanExp
pType t = satisfy (\case
                     (CleanType n) -> t == n
                     _ -> False)

pArr :: Parser CleanExp
pArr = pVar "->"

--- utilities
isSexp CleanSexp{} = True
isSexp _ =           False

isType CleanType{} = True
isType _ =           False

isLit CleanLit{} = True
isLit _ =          False

isVar CleanVar{} = True
isVar _ =          False
