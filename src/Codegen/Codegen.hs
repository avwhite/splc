module Codegen.Codegen where

import Parser.AST

import Control.Monad.State

import Data.Traversable
import Data.Maybe

data Instr = Add | Mul | Ldc Integer | Label String | Bsr String
    | Bra String | Link Integer | Unlink | Ret | Stl Integer
    | StrRR | LdrRR | Ajs Integer deriving (Show)

data VarCtx = VarCtx [(Identifier, Integer)]

type Codegen a = State VarCtx a

lookupVar :: String -> Codegen Integer
lookupVar id = do
    (VarCtx l) <- get
    pure $ fromJust (lookup id l) --Should this be fixed?


genCode :: Codegen [Instr] -> [Instr]
genCode cg = evalState cg (VarCtx [])

codeGenExp :: ASTExp -> Codegen [Instr]
codeGenExp (IntE i) = pure [Ldc i]
codeGenExp (Op2E Plus e1 e2) = fmap mconcat $ sequence
    [ codeGenExp e1
    , codeGenExp e2
    , pure [Add]
    ]
codeGenExp (Op2E Times e1 e2) = fmap mconcat $ sequence
    [ codeGenExp e1
    , codeGenExp e2
    , pure [Mul]
    ]

codeGenStmt _ (AssignS id [] e) = do
    exprCode <- codeGenExp e
    --For assignments to fields we need to calculate an addr to write to
    location <- lookupVar id
    pure (exprCode ++ [Stl location])
codeGenStmt _ (FunCallS id args) = do
    argCode <- fmap mconcat $ sequence (fmap codeGenExp args)
    pure (argCode ++ [Bsr id, Ajs (toInteger (-(length args))),LdrRR])
codeGenStmt retLabel (ReturnS (Just e)) = fmap mconcat $ sequence
    [ codeGenExp e
    , pure [StrRR, Bra retLabel]
    ]
codeGenStmt retLabel (ReturnS Nothing) = pure $ [Bra retLabel]

handleDecls :: [ASTVarDecl] -> Codegen Integer
handleDecls ((VarDecl _ id _):ds) = do
    rest <- handleDecls ds
    modify (\(VarCtx s) -> VarCtx ((id, rest):s))
    pure (rest + 1)
handleDecls [] = pure 0

codeGenFunDecl :: ASTFunDecl -> Codegen [Instr]
codeGenFunDecl (FunDecl id args _ decls body) = do
    --Puts locals in the context and returns amount of space needed.
    ctx <- get
    space <- handleDecls decls
    prologue <- pure [Label id, Link space]
    init <- pure [] --codeGenVarDecls decls
    let retLabel = id ++ "$return"
    main  <- fmap mconcat $ sequence (fmap (codeGenStmt retLabel) body)
    epilouge <- pure [Label retLabel, Unlink, Ret]
    put ctx -- restore context
    pure $ mconcat [prologue, init, main, epilouge]

--codeGenASTVarDecl :: ASTVarDecl -> Codegen [Instr]
--codeGEnASTVarDecl (VarDecl _ id e) =
    --Reserve stack location (requires code)
    --Save location in ctx
    --Evaluate expression and assign var. (requires code)

codeGen :: AST -> Codegen [Instr]
codeGen (AST (FunD f:rs)) = do
    funCode  <- codeGenFunDecl f
    restCode <- codeGen (AST rs)
    pure (funCode ++ restCode)
codeGen (AST (VarD _:rs)) = codeGen (AST rs) --ignore global variables for now
codeGen (AST []) = pure []
