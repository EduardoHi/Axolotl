{-# LANGUAGE DeriveDataTypeable #-}
module Axo.ParseTree where


import Data.Data

data Literal = IntLit String
             | FloatLit String
             | StringLit String
             | CharLit Char
             deriving (Show, Eq, Data, Typeable, Ord)

-- TODO: if we relax the Program definition of not only one space, and
-- not only sexps, Program and ExpSeq are equivalent
-- and things like comment handling is easier
data Program = Program [Exp] deriving (Show, Eq, Data, Typeable)

data Identifier = VarId String
                | TypeId String
                deriving (Show, Eq, Data, Typeable)

data Atom = Id Identifier
          | Literal Literal
          deriving (Show, Eq, Data, Typeable)

data Sexp = Sexp ExpSeq deriving (Show, Eq, Data, Typeable)

data Exp = ESexp Sexp
         | EAtom Atom
         | EIexp Iexp
         | EInfixexp InfixExp
         | EComment Comment
         deriving (Show, Eq, Data, Typeable)

data ExpSeq = ExpSeq [Exp] deriving (Show, Eq, Data, Typeable)

data Iexp = Iexp ExpSeq [ExpSeq] deriving (Show, Eq, Data, Typeable)

data InfixExp = InfixExp Exp Exp Exp deriving (Show, Eq, Data, Typeable)

-- Comments might appear inside a sequence of expressions, or at the top level of a program
data Comment = Comment String deriving (Show, Eq, Data, Typeable)


-- Desugared ParseTree extra types, this types should not be used inside the Parser
-- unless there is a good reason for it

newtype CleanProgram = CleanProgram [CleanExp] deriving (Show, Eq, Data, Typeable)

data CleanExp = CleanSexp [CleanExp]
              | CleanLit Literal
              | CleanVar String
              | CleanType String
              deriving (Show, Eq, Data, Typeable, Ord)

-- | joins the inner exps of ExpSeq
joinExpSeqs :: [ExpSeq] -> ExpSeq
joinExpSeqs xs = ExpSeq $ foldMap (\(ExpSeq e) -> e) xs


constrName :: Data a => a -> String
constrName = showConstr . toConstr

