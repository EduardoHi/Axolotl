{-# LANGUAGE OverloadedStrings #-}
module Axo.PrettyPrinter where

import Axo.ParseTree
import qualified Axo.AST as AST
import Data.List
import Text.PrettyPrint
import Prelude hiding ((<>))


-- this is the pretty printer for the Parser Tree, not the AST
-- TODO: add AST pretty printing

class Pretty x where
  pretty :: x -> Doc


prettyText :: Pretty a => a -> String
prettyText = render . pretty

-- Token pretty printing --

instance Pretty Atom where
  pretty a = case a of
    (Id i) -> pretty i
    (Literal l) -> pretty l

instance Pretty Literal where
  pretty l = case l of
    (IntLit s) -> text s
    (FloatLit s) -> text s
    (StringLit s) -> text $ show s
    (CharLit c) -> text $ show c

instance Pretty Identifier where
  pretty i = case i of
    (VarId v) -> text v
    (TypeId t) -> text t

instance Pretty Comment where
  pretty (Comment c) = (text "--") <> (text c) <> (text "\n")


-- Expressions pretty printing --    

instance Pretty ExpSeq where
  pretty (ExpSeq []) = error "ExpSequence should not be empty"
  pretty (ExpSeq (e:es)) = pretty e <+> (nest 4 $ sep $ map pretty es)

instance Pretty Exp where
  pretty (ESexp s) = pretty s
  pretty (EAtom a) = pretty a
  pretty (EIexp i) = pretty i
  pretty (EInfixexp i) = pretty i
  pretty (EComment c) = pretty c

instance Pretty InfixExp where
  pretty (InfixExp a b c) = braces $ (pretty a) <+> (pretty b) <+> (pretty c)

instance Pretty Iexp where
  pretty (Iexp head block) = (pretty head) $+$ (nest 4 $ sep $ map pretty block)
  
instance Pretty Sexp where
  pretty (Sexp es) = parens (pretty es)

instance Pretty Program where
  pretty (Program es) = vcat $ map pretty es


-- Desugared Parse Tree pretty printing -- 

instance Pretty CleanProgram where
  pretty (CleanProgram es) = vcat $ map pretty es

instance Pretty CleanExp where
  pretty (CleanSexp []) = error "CleanSexp should not be empty"
  pretty (CleanSexp (e:es)) = parens (pretty e <+> (nest 4 $ hsep $ map pretty es))
  pretty (CleanEAtom a) = pretty a

-- -- AST pretty printing --

-- TODO: Fix this pretty printing to proper pretty printing !! (i.e. should represent code, and not just a show to the data typecc)

instance Pretty AST.Lit where
  pretty = text . show

instance Pretty AST.Expr where
  pretty = text . show

instance Pretty AST.Program where
  pretty = text . show
