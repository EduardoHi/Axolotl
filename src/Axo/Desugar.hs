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
    (ESexp s) -> CleanESexp $ desugar s
    (EAtom a) -> CleanEAtom a
    (EIexp i) -> CleanESexp $ desugar i
    (EInfixexp i) -> CleanESexp $ desugar i


instance Desugar Iexp CleanSexp where
  desugar (Iexp header expseqs) = CleanSexp $ desugar $ joinExpSeqs (header:expseqs)

-- the generalized Infix desugaring is more complex
instance Desugar InfixExp CleanSexp where
  -- note that we change the order of a b c, to b, a, c
  desugar (InfixExp a b c) = (CleanSexp . CleanExpSeq) $ map desugar [b, a, c]

instance Desugar Sexp CleanSexp where
  desugar (Sexp expseq) = CleanSexp $ desugar expseq

instance Desugar ExpSeq CleanExpSeq where
  desugar (ExpSeq exps) = CleanExpSeq $ map desugar $ lefts exps

