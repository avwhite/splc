module Codegen.Codegen where

import Parser.AST

import Control.Monad.State
import Data.Traversable

data Instr = Add | Mul | Ldc Integer deriving (Show)

data VarCtx = VarCtx [(Identifier, Integer)]

type Codegen a = State VarCtx a

genCode :: Codegen [Instr] -> [Instr]
genCode cg = evalState cg (VarCtx [])

codeGenExp :: ASTExp -> Codegen [Instr]
codeGenExp (IntE i) = pure [Ldc i]
codeGenExp (Op2E Plus e1 e2) = fmap mconcat $ sequence
    [ codeGenExp e1
    , codeGenExp e2
    , pure [Add]
    ]
