module AxoPrettyPrinter where

-- this is the pretty printer for the Parser Tree, not the AST
-- TODO: add AST pretty printing

import AxoParser





-- Token pretty printing --

-- prettyPrinting :: (a node of the Parser Tree) -> String

ppAtom (Id s) = show s
ppAtom (Literal l) = ppLiteral l


ppLiteral lit = case lit of
  (IntLit s) -> s
  (FloatLit s) -> s
  -- this should probably be show s to handle \ cases
  (StringLit s) -> "\"" ++ s ++ "\""
  (CharLit c) -> show c



