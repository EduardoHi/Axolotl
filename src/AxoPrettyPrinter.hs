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
  (StringLit s) -> show s
  (CharLit c) -> show c



