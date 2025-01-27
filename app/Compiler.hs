{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Compiler where

import           Data.List            (intercalate)
import qualified Data.Set             as S

import           Control.Monad.Except
import           Control.Monad.State

import qualified Axo.AST              as AST (Expr, Program (..), Type)
import           Axo.Check            (TypeEnv, checkProgram, checkTop,
                                       emptyTypeEnv, typeErrorPretty)
import           Axo.Desugar          (desugar)
import           Axo.Match            (matchProgram, matchTop)
import           Axo.Parser           (parseExpr, parseProgram)
import           Axo.ParseTree        (CleanExp, CleanProgram (..),
                                       Program (..))
import           Axo.PrettyPrinter    (Pretty, prettyText)
import           Axo.ToAST            (toAST)
import           Axo.ToGraph          (ToGraph, showGraph, toGraph)

import           Flags

data Phase
  = PAst AST.Program
  | PDesugar CleanProgram
  | PSt Program
  deriving (Show)

data CompilerState = CompilerState
  {  _filename :: String
  ,  _flags    :: Flags
  ,  _source   :: String             -- todo: change to Text
  ,  _st       :: Maybe Program      -- syntax tree
  , _desugared :: Maybe CleanProgram -- desugared syntax tree
  , _ast       :: Maybe AST.Program  -- abstract syntax tree
  , _tyenv     :: TypeEnv            -- typing environment
  , _pmast     :: Maybe AST.Program  -- pattern matched abstract syntax tree
  -- ... extra info: a var table... etc.
  } deriving Show

emptyState = CompilerState
  {  _filename = mempty
  ,  _flags    = S.empty
  ,  _source   = mempty
  ,  _st       = Nothing
  , _desugared = Nothing
  , _ast       = Nothing
  , _tyenv     = emptyTypeEnv
  , _pmast     = Nothing
  }

type CompilerMonad =
  ExceptT String -- Exception of Type String
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


loadExpr :: String -> CompilerM (AST.Expr, AST.Type)
loadExpr s = do
  modify (\x -> x {_source = s})
  (CleanProgram exps) <- tParseE >>= tDesugar
  case exps of
    [] -> throwError "Empty Program"
    _  -> do
      e <- tDesugarE (head exps)
      et <- tCheckE e
      e' <- tMatchE e
      return (e',et)

loadModule :: String -> CompilerM AST.Program
loadModule s = do
  modify (\x -> x {_source = s})
  a <- tParseP >>= tDesugar >>= tAST
  tCheckP a
  a' <- tMatchP a
  --- later phases here might be Intermediate Representation, CodeGen, etc.
  phase <- choosePhase
  case phase of
    Just p -> case p of
                (PAst x)     -> output x
                (PDesugar x) -> output x
                (PSt x)      -> output x
    Nothing -> throwError "Compiler: nothing to output"
  return a'
  where output x = do
          ifFlag OGraph $ graphOut x
          ifFlag OHaskellData $ haskellDataOut x
          ifFlag OAxolotlSrc $ axolotlSrcOut x

loadFile :: String -> CompilerM AST.Program
loadFile filename = do
  sourceCode <- liftIO $ readFile $ filename
  loadModule sourceCode

--- Transformations

-- | Parses a Program from source
tParseP :: CompilerM Program
tParseP = do
  src <- gets _source
  case parseProgram src of
    Left e -> throwError e
    Right st -> do
      modify (\x -> x {_st = Just st})
      return st

-- | Parses an Expression from source, returns a Program with only that expression
tParseE :: CompilerM Program
tParseE = do
  src <- gets _source
  case parseExpr src of
    Left e -> throwError e
    Right st -> do
      modify (\x -> x {_st = Just (Program [st])})
      return (Program [st])

-- | Desugars the parsed program
tDesugar :: Program -> CompilerM CleanProgram
tDesugar p = do
  let desugared = desugar p
  modify (\x -> x {_desugared = Just desugared})
  return desugared

-- | Transforms a clean expression to an AST
tDesugarE :: CleanExp -> CompilerM AST.Expr
tDesugarE e = do
  case toAST e of
    Left e    -> throwError $ intercalate "\n\n" e
    Right ast -> do
      modify (\x -> x {_ast = Just (AST.Program [ast])})
      return ast

-- | Transforms a clean Program to an AST
tAST :: CleanProgram -> CompilerM AST.Program
tAST p = do
  case toAST p of
    Left e    -> throwError $ intercalate "\n\n" e
    Right ast -> do
      modify (\x -> x {_ast = Just ast})
      return ast

-- | Type Check that the Program is well-typed
tCheckP :: AST.Program -> CompilerM TypeEnv
tCheckP (AST.Program exps) = do
  tyenv <- gets _tyenv
  case checkProgram tyenv exps of
    Left e  -> throwError $ typeErrorPretty e
    Right (_, tyenv') -> do
      modify (\x -> x {_tyenv = tyenv'})
      return tyenv'

-- | Type Check that the Expression is well-typed
tCheckE :: AST.Expr -> CompilerM AST.Type
tCheckE e = do
  tyenv <- gets _tyenv
  case checkTop tyenv e of
    Left e   -> throwError $ typeErrorPretty e
    Right (t, tyenv') -> do
      modify (\x -> x {_tyenv = tyenv'})
      return t

-- | Transform Function Definitions with Pattern Matching to Case Exprs
tMatchP :: AST.Program -> CompilerM AST.Program
tMatchP p = do
  tyenv <- gets _tyenv
  let ast = matchProgram tyenv p
  modify (\x -> x {_pmast = Just ast})
  return ast

-- | Transform the expresssion from a Function Definition with Pattern Matching to a Case Expr
tMatchE :: AST.Expr -> CompilerM AST.Expr
tMatchE e = do
  tyenv <- gets _tyenv
  let e' = matchTop tyenv e
  modify (\x -> x {_pmast = Just (AST.Program [e'])})
  return e'

--- End Transformations

-- | graph viz format output to a file
graphOut :: ToGraph t => t -> CompilerM ()
graphOut x = genericOut x (showGraph . toGraph) ".dot"

-- | haskell data format output to a file
haskellDataOut :: (Show t) => t -> CompilerM ()
haskellDataOut field = genericOut field show ".data.hs"

-- | axolotl source format output to a file. Basically a pretty printer
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
           _          -> PAst <$> a -- by default, return ast
