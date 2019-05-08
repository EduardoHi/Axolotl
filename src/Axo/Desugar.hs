{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Axo.Desugar where

import Axo.ParseTree
import Data.Maybe(catMaybes)

class Desugar p q where
  desugar :: p -> q


instance Desugar Program CleanProgram where
  desugar (Program exps) = CleanProgram $ desugar exps

instance Desugar Atom CleanExp where
  desugar (Id i) =
    case i of
      (VarId ":")  -> CleanType "Cons"
      (VarId "[]") -> CleanType "Nil"
      (VarId ",")  -> CleanType "Tuple"
      (VarId  s)   -> CleanVar  s
      (TypeId s)   -> CleanType s
  desugar (Literal li) = CleanLit li

instance Desugar Exp (Maybe CleanExp) where
  desugar e =
    case e of
      (EComment _)  -> Nothing
      (ESexp s)     -> Just $ desugar s
      (EAtom a)     -> Just $ desugar a
      (EInfixexp i) -> Just $ desugar i

instance Desugar [Exp] [CleanExp] where
  desugar es = catMaybes $ map desugar es

-- the generalized Infix desugaring is more complex
instance Desugar InfixExp CleanExp where
  -- note that we change the order of a b c, to b, a, c
  desugar (InfixExp a b c) = CleanSexp $ desugar [b, a, c]

instance Desugar Sexp CleanExp where
  desugar (Sexp expseq) = CleanSexp $ desugar expseq

instance Desugar ExpSeq [CleanExp] where
  desugar (ExpSeq exps) = desugar exps

