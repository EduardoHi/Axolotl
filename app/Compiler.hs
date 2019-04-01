{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Compiler where

import qualified Data.Set as S

import Control.Monad.Except
import Control.Monad.State

import Axo.PrettyPrinter (prettyText, Pretty, pretty)
import Axo.Parser (parseProgram')
import Axo.ToGraph (showGraph, toGraph, ToGraph, toNode)
import Axo.ParseTree (Program, CleanProgram)
import Axo.Desugar (desugar)
import qualified Axo.AST as AST (toAST, Program)

import Flags

data Phase
  = PAst AST.Program
  | PDesugar CleanProgram
  | PSt Program

instance ToGraph Phase where
  toNode (PAst a) = toNode a
  toNode (PDesugar d) = toNode d
  toNode (PSt s) = toNode s

instance Show Phase where
  show (PAst a) = show a
  show (PDesugar d) = show d
  show (PSt s) = show s

instance Pretty Phase where
  pretty (PAst a) = pretty a
  pretty (PDesugar d) = pretty d
  pretty (PSt s) = pretty s


data CompilerState = CompilerState
  {  _filename :: String
  ,  _flags    :: Flags
  ,  _source   :: String -- todo: change to Text god damn
  ,  _st       :: Maybe Program -- syntax tree
  , _desugared :: Maybe CleanProgram
  , _ast       :: Maybe AST.Program -- abstract syntax tree
  } deriving Show

emptyState = CompilerState
  {  _filename = mempty
  ,  _flags    = S.empty
  ,  _source   = mempty
  ,  _st       = Nothing
  , _desugared = Nothing
  , _ast       = Nothing
  }

type CompilerMonad =
  ExceptT String
  (StateT CompilerState IO)


newtype CompilerM a = Compiler { runCompiler :: CompilerMonad a }
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadFix
  , MonadError String
  , MonadIO
  , MonadState CompilerState
  )

runCompilerM
  :: CompilerM a
  -> CompilerState
  -> IO (Either String a, CompilerState)
runCompilerM = runStateT . runExceptT . runCompiler

inIO :: IO a -> CompilerM a
inIO = Compiler . liftIO

ifFlag :: Flag -> CompilerM a -> CompilerM ()
ifFlag flag m = do 
  flags <- gets _flags
  when (flags `isSet` flag) (void m)


pipeline :: String -> CompilerM AST.Program
pipeline s = do
  modify (\x -> x {_source = s})
  a <- tParse >>= tDesugar >>= tAST
  phase <- choosePhase
  case phase of
    Just p -> case p of
                (PAst x) -> output x
                (PDesugar x) -> output x
                (PSt x) -> output x
    Nothing -> throwError "Compiler: nothing to output"
  return a
  where output x = do
          ifFlag OGraph $ graphOut x
          ifFlag OHaskellData $ haskellDataOut x
          ifFlag OAxolotlSrc $ axolotlSrcOut x

tDesugar :: Program -> CompilerM CleanProgram
tDesugar p = do
  let desugared = desugar p
  modify (\x -> x {_desugared = Just desugared})
  return desugared

tAST :: CleanProgram -> CompilerM AST.Program
tAST p = do
  let ast = AST.toAST p
  modify (\x -> x {_ast = Just ast})
  return ast

tParse :: CompilerM Program
tParse = do
  src <- gets _source
  case parseProgram' src of
    Left e -> throwError e
    Right st -> do
      modify (\x -> x {_st = Just st})
      return st

graphOut :: ToGraph t => t -> CompilerM ()
graphOut x = genericOut x (showGraph . toGraph) ".dot"

haskellDataOut :: (Show t) => t -> CompilerM ()
haskellDataOut field = genericOut field show ".data.hs"

axolotlSrcOut :: (Pretty t) => t -> CompilerM ()
axolotlSrcOut field = genericOut field prettyText ".pretty.axo"

genericOut :: t -> (t -> String) -> String -> CompilerM ()
genericOut value g ext = do
  file  <- gets _filename
  inIO $ writeFile (file++ext) $ (g value) ++ "\n"


choosePhase :: CompilerM (Maybe Phase)
choosePhase = do
  flags <- gets _flags
  a  <- gets _ast
  d  <- gets _desugared
  s <- gets _st
  let isOn = isSet flags  
  return $ case (isOn SAst, isOn SDesugar, isOn SParse) of
           (True,_,_) -> PAst <$> a
           (_,True,_) -> PDesugar <$> d
           (_,_,True) -> PSt <$> s
           _ -> PAst <$> a -- by default, return ast
