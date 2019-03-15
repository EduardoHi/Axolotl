module Axo.PrettyPrinter where

import Axo.ParseTree
import Axo.Parser
import Data.List


-- this is the pretty printer for the Parser Tree, not the AST
-- TODO: add AST pretty printing



-- Token pretty printing --

instance PrettyPrint Atom where
  pprint a = case a of
    (Id i) -> pprint i
    (Literal l) -> pprint l

instance PrettyPrint Literal where
  pprint l = case l of
    (IntLit s) -> s
    (FloatLit s) -> s
    (StringLit s) -> (show s)
    (CharLit c) -> show c

instance PrettyPrint Identifier where
  pprint i = case i of
    (VarId v) -> v
    (TypeId t) -> t

instance PrettyPrint Comment where
  pprint (Comment c) = "--" ++ c ++ "\n"


-- Expressions pretty printing --    

instance PrettyPrint ExpSeq where
  pprint (ExpSeq es) = intercalate " " $ map (either pprint pprint) es

instance PrettyPrint Exp where
  pprint (ESexp s) = pprint s
  pprint (EAtom a) = pprint a

instance PrettyPrint Sexp where
  pprint (Sexp es) = "(" ++ (pprint es) ++ ")"

class PrettyPrint x where
  pprint :: x -> String
