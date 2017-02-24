{-# LANGUAGE DeriveDataTypeable #-}
module Parser.AST where

import Data.Data

type Identifier = String

data ASTType =
      BoolT
    | IntT
    | CharT
    | PairT ASTType ASTType
    | ListT ASTType
    | PolyT Identifier deriving (Data, Show)
data ASTReturnType =
      Void
    | ReturnType ASTType deriving (Data, Show)
data ASTFunType = FunType [ASTType] ASTReturnType deriving (Data, Show)

data Op1 = Neg | Not deriving (Data, Show)

data Op2 =
      Plus | Minus | Times | Div | Mod
    | Equal | Less | Greater | LessEq | GreaterEq | NotEq
    | And | Or
    | Cons deriving (Data, Show)

type Precedence = Int
data Associativity = LeftAssoc | RightAssoc

highestPrecedence = 4 :: Precedence

precedence :: Op2 -> Precedence
precedence Equal = 0
precedence NotEq = 0
precedence Less = 1
precedence Greater = 1
precedence LessEq = 1
precedence GreaterEq = 1
precedence Cons = 2
precedence Plus = 3
precedence Minus = 3
precedence Or = 3
precedence Times = 4
precedence Div = 4
precedence Mod = 4
precedence And = 4

assoc :: Precedence -> Associativity
assoc 2 = RightAssoc
assoc _ = LeftAssoc

data Field = Hd | Tl | Fst | Snd deriving (Data, Show)

data ASTExp =
      Var Identifier [Field]
    | Op2E Op2 ASTExp ASTExp
    | Op1E Op1 ASTExp
    | FunCallE Identifier [ASTExp]
    | IntE Int
    | CharE Char
    | BoolE Bool
    | NilE
    | PairE ASTExp ASTExp deriving (Data, Show)

data ASTStmt =
      IfS ASTExp [ASTStmt] (Maybe [ASTStmt])
    | WhileS ASTExp [ASTStmt]
    | AssignS Identifier ASTExp
    | FunCallS Identifier [ASTExp]
    | ReturnS (Maybe ASTExp) deriving (Data, Show)
