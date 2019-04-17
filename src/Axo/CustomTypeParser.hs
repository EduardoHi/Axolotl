{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE LambdaCase        #-}
module Axo.CustomTypeParser where
  
import Data.Proxy
import Data.Void
import qualified Data.List          as DL
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set

import Text.Megaparsec


data AToken
  = Int Int
  | OParen
  | CParen
  | Plus
  | Mult
  deriving (Eq, Ord, Show)
  
newtype ATokenStream = ATokenStream { unTokenStream :: [AToken] }

instance Stream ATokenStream where
  type Token  ATokenStream = AToken
  type Tokens ATokenStream = [AToken]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs

  chunkToTokens Proxy = id

  chunkLength Proxy = length

  chunkEmpty Proxy = null

  take1_ (ATokenStream []) = Nothing
  take1_ (ATokenStream (t:ts)) = Just (t, ATokenStream ts)

  takeN_ n (ATokenStream s)
    | n <= 0    = Just ([], ATokenStream s)
    | null s    = Nothing
    | otherwise =
        let (x, s') = splitAt n s
        in Just (x, ATokenStream s')

  takeWhile_ f (ATokenStream s) =
    let (x, s') = DL.span f s
    in (x, ATokenStream s')

  showTokens Proxy = DL.intercalate ", "
    . NE.toList
    . fmap showToken

  reachOffset o pst@PosState {..} =
    case drop (o - pstateOffset) (unTokenStream pstateInput) of
      [] ->
        ( pstateSourcePos
        , "<missing input>"
        , pst { pstateInput = ATokenStream [] }
        )
      (x:xs) ->
        ( pstateSourcePos
        , "<missing input>"
        , pst { pstateInput = ATokenStream (x:xs) }
        )
        

showToken = \case
  (Int i) -> show i
  OParen  -> "("
  CParen  -> ")"
  Plus    -> "+"
  Mult    -> "*"


type Parser = Parsec Void ATokenStream

pToken :: AToken -> Parser AToken
pToken c = token test (Set.singleton . Tokens . nes $ c)
  where
    test x = if x == c
             then Just x
             else Nothing
    nes x = x NE.:| []

pInt :: Parser Int
pInt = token test Set.empty <?> "integer"
  where
    test (Int n) = Just n
    test _ = Nothing


pSum :: Parser (Int, Int)
pSum = do
  a <- pInt
  _ <- pToken Plus
  b <- pInt
  return (a, b)


exampleStream :: ATokenStream
exampleStream = ATokenStream
  [ (Int 5)
  , Mult         -- (1)
  , (Int 6) ]
