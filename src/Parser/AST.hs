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

data Op2 =
      Plus | Minus | Times | Div | Mod
    | Equal | Less | Greater | LessEq | GreaterEq | NotEq
    | And | Or
    | Cons
--TODO: Add Fields, Op1, FunCall
data ASTExp =
      Var Identifier
    | Op2E Op2 ASTExp ASTExp
    | IntE Int
    | CharE Char
    | BoolE Bool
    | NilE
    | PairE ASTExp ASTExp
    | ParanE ASTExp
