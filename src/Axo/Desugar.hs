{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Axo.Desugar where

import Axo.ParseTree
import Data.Maybe(catMaybes)

class Desugar p q where
  desugar :: p -> q


instance Desugar Program CleanProgram where
  desugar (Program exps) = CleanProgram $ desugar exps

instance Desugar Exp (Maybe CleanExp) where
  desugar e = case e of
    (ESexp s) -> Just $ desugar s
    (EAtom a) -> Just $ CleanEAtom a
    (EIexp i) -> Just $ desugar i
    (EInfixexp i) -> Just $ desugar i
    (EComment _) -> Nothing

instance Desugar [Exp] [CleanExp] where
  desugar es = catMaybes $ map desugar es

instance Desugar Iexp CleanExp where
  desugar (Iexp header expseqs) = CleanSexp $ desugar $ joinExpSeqs (header:expseqs)

-- the generalized Infix desugaring is more complex
instance Desugar InfixExp CleanExp where
  -- note that we change the order of a b c, to b, a, c
  desugar (InfixExp a b c) = CleanSexp $ desugar [b, a, c]

instance Desugar Sexp CleanExp where
  desugar (Sexp expseq) = CleanSexp $ desugar expseq

instance Desugar ExpSeq [CleanExp] where
  desugar (ExpSeq exps) = desugar exps

