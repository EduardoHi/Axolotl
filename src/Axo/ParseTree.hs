module Axo.ParseTree where


data Literal = IntLit String
             | FloatLit String
             | StringLit String
             | CharLit Char
             deriving (Show, Eq)


-- TODO: if we relax the Program definition of not only one space, and
-- not only sexps, Program and ExpSeq are equivalent
-- and things like comment handling is easier
data Program = Program [Either Exp Comment] deriving (Show, Eq)

data Identifier = VarId String
                | TypeId String
                deriving (Show, Eq)

data Atom = Id Identifier
          | Literal Literal
          deriving (Show, Eq)

data Sexp = Sexp ExpSeq deriving (Show, Eq)

data Exp = ESexp Sexp
         | EAtom Atom
         | EIexp Iexp
         | EInfixexp InfixExp
         deriving (Show, Eq)

data ExpSeq = ExpSeq [Either Exp Comment] deriving (Show, Eq)

data Iexp = Iexp ExpSeq [ExpSeq] deriving (Show, Eq)

data InfixExp = InfixExp Exp Exp Exp deriving (Show, Eq)

-- Comments might appear inside a sequence of expressions, or at the top level of a program
data Comment = Comment String deriving (Show, Eq)


-- Desugared ParseTree extra types, this types should not be used inside the Parser
-- unless there is a good reason for it

newtype CleanProgram = CleanProgram [CleanExp] deriving (Show, Eq)

data CleanExp = CleanSexp [CleanExp]
              | CleanEAtom Atom
              deriving (Show, Eq)

-- | joins the inner exps of ExpSeq
joinExpSeqs :: [ExpSeq] -> ExpSeq
joinExpSeqs xs = ExpSeq $ foldMap (\(ExpSeq e) -> e) xs

