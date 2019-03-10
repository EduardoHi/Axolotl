module Axo.PrettyPrinter where

-- this is the pretty printer for the Parser Tree, not the AST
-- TODO: add AST pretty printing

import Axo.Parser





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


