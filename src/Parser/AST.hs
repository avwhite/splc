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
