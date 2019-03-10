module Axo.PrettyPrinter where

-- this is the pretty printer for the Parser Tree, not the AST
-- TODO: add AST pretty printing

import Axo.Parser
import Data.List




-- Token pretty printing --

-- prettyPrinting :: (a node of the Parser Tree) -> String

ppAtom :: Atom -> String
ppAtom (Id s) = ppIdentifier s
ppAtom (Literal l) = ppLiteral l

ppLiteral :: Literal -> String
ppLiteral lit = case lit of
  (IntLit s) -> s
  (FloatLit s) -> s
  -- this should probably be show s to handle \ cases
  (StringLit s) -> "\"" ++ s ++ "\""
  (CharLit c) -> show c

ppIdentifier :: Identifier -> String
ppIdentifier (VarId i) = i
ppIdentifier (TypeId i) = i

ppExpSeq :: ExpSeq -> String
ppExpSeq (ExpSeq es) = intercalate " " $ map (either pprint pprint) es

-- instance PrettyPrint Sexp where
--   pprint (Sexp s) = 

instance PrettyPrint Comment where
  pprint (Comment c) = "--" ++ c ++ "\n"

instance PrettyPrint Exp where
  -- replace this with ppExpSeq
  pprint (ESexp s) = pprint s
  --TODO change ppAtom to a well formed instance of Pretty Print for atom
  pprint (EAtom a) = ppAtom a

instance PrettyPrint Sexp where
  pprint (Sexp es) = "("++ (ppExpSeq es)++ ")"

class PrettyPrint x where
  pprint :: x -> String
