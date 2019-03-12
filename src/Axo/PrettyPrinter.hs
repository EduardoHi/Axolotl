module Axo.PrettyPrinter where

-- this is the pretty printer for the Parser Tree, not the AST
-- TODO: add AST pretty printing

import Axo.Parser
import Data.List




-- Token pretty printing --

-- prettyPrinting :: (a node of the Parser Tree) -> String

-- ppAtom :: Atom -> String
-- ppAtom (Id s) = ppIdentifier s
-- ppAtom (Literal l) = ppLiteral l

-- ppLiteral :: Literal -> String
-- ppLiteral lit = case lit of
--   (IntLit s) -> s
--   (FloatLit s) -> s
--   -- this should probably be show s to handle \ cases
--   (StringLit s) -> "\"" ++ s ++ "\""
--   (CharLit c) -> show c

-- ppIdentifier :: Identifier -> String
-- ppIdentifier (VarId i) = i
-- ppIdentifier (TypeId i) = i

-- ppExpSeq :: ExpSeq -> String
-- ppExpSeq (ExpSeq es) = intercalate " " $ map (either pprint pprint) es

instance PrettyPrint ExpSeq where
  pprint (ExpSeq es) = intercalate " " $ map (either pprint pprint) es

instance PrettyPrint Comment where
  pprint (Comment c) = "--" ++ c ++ "\n"

instance PrettyPrint Exp where
  pprint (ESexp s) = pprint s
  pprint (EAtom a) = pprint a

instance PrettyPrint Sexp where
  pprint (Sexp es) = "(" ++ (pprint es) ++ ")"

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

class PrettyPrint x where
  pprint :: x -> String
