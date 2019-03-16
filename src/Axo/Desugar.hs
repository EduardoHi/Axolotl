{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Axo.Desugar where

import Axo.ParseTree
import Data.Either

class Desugar p q where
  desugar :: p -> q


instance Desugar Program CleanProgram where
  desugar (Program exps) = CleanProgram $ map desugar $ lefts exps

instance Desugar Exp CleanExp where
  desugar e = case e of
    (ESexp s) -> desugar s
    (EAtom a) -> CleanEAtom a
    (EIexp i) -> desugar i
    (EInfixexp i) -> desugar i


instance Desugar Iexp CleanExp where
  desugar (Iexp header expseqs) = CleanSexp $ desugar $ joinExpSeqs (header:expseqs)

-- the generalized Infix desugaring is more complex
instance Desugar InfixExp CleanExp where
  -- note that we change the order of a b c, to b, a, c
  desugar (InfixExp a b c) = CleanSexp $ map desugar [b, a, c]

instance Desugar Sexp CleanExp where
  desugar (Sexp expseq) = CleanSexp $ desugar expseq

instance Desugar ExpSeq [CleanExp] where
  desugar (ExpSeq exps) = map desugar $ lefts exps

