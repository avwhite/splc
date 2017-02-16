{-# LANGUAGE DeriveDataTypeable #-}
module Parser.AST where

import Data.Data

type Identifier = String
data ASTType = BoolT | IntT | CharT | PairT ASTType ASTType | ListT ASTType
    | PolyT Identifier deriving (Data, Show)
data ASTReturnType = Void | ReturnType ASTType deriving (Data, Show)
data ASTFunType = FunType [ASTType] ASTReturnType deriving (Data, Show)
