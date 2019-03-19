module Axo.PrettyPrinter where

import Axo.ParseTree
import qualified Axo.AST as AST
import Data.List


-- this is the pretty printer for the Parser Tree, not the AST
-- TODO: add AST pretty printing

class PrettyPrint x where
  pprint :: x -> String


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
  pprint (ExpSeq es) = intercalate " " $ map pprint es

instance PrettyPrint Exp where
  pprint (ESexp s) = pprint s
  pprint (EAtom a) = pprint a
  -- TODO : pprint EIexp
  -- TODO : pprint EInfixexp
  pprint (EComment c) = pprint c

instance PrettyPrint Sexp where
  pprint (Sexp es) = "(" ++ (pprint es) ++ ")"


instance PrettyPrint Program where
  pprint (Program es) = intercalate "\n" $ map pprint es


-- Desugared Parse Tree pretty printing -- 

instance PrettyPrint CleanProgram where
  pprint (CleanProgram es) = intercalate "\n" $ map pprint es

instance PrettyPrint CleanExp where
  pprint (CleanSexp e) = "(" ++ (intercalate " " $ map pprint e) ++ ")"
  -- TODO : pprint CleanEAtom
  

-- -- AST pretty printing --

instance PrettyPrint AST.Lit where
  pprint = show

instance PrettyPrint AST.Expr where
  pprint = show

instance PrettyPrint AST.Program where
  pprint = show
