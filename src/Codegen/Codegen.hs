module Codegen.Codegen where

import Parser.AST

import Control.Monad.State

import Data.Traversable
import Data.Maybe

data Instr = Halt | Ldc Integer | Ldl Integer | Label String | Bsr String
    | Bra String | Brf String
    | Link Integer | Unlink | Ret | Stl Integer
    | StrRR | LdrRR | Ajs Integer
    | Ldh Integer | Stmh Integer | Sta Integer
    | Add | Mul | Sub | DivI | ModI | AndI | OrI
    | Eq | Ne | Lt | Gt | Le | Ge
    | Neg | Not
    | Trap Integer deriving (Show)

data VarCtx = VarCtx [(Identifier, Integer)]

type Codegen a = State (VarCtx, Integer) a

lookupVar :: String -> Codegen Integer
lookupVar id = do
    (VarCtx l, _) <- get
    pure $ fromJust (lookup id l) --Should this be fixed?

insertCtx :: (String, Integer) -> Codegen ()
insertCtx a = modify (\(VarCtx s, u) -> (VarCtx (a:s), u))

newUnique :: Codegen Integer
newUnique = do
    (ctx, u) <- get
    put (ctx, u+1)
    pure u

genCode :: Codegen [Instr] -> [Instr]
genCode cg = evalState cg ((VarCtx []), 0)

opToInstr Plus = Add
opToInstr Minus = Sub
opToInstr Times = Mul
opToInstr Div = DivI
opToInstr Mod = ModI
opToInstr And = AndI
opToInstr Or = OrI
opToInstr Equal = Eq
opToInstr NotEq = Ne
opToInstr Less = Lt
opToInstr Greater = Gt
opToInstr LessEq = Le
opToInstr GreaterEq = Ge

op1ToInstr Parser.AST.Neg = Codegen.Codegen.Neg
op1ToInstr Parser.AST.Not = Codegen.Codegen.Not

codeGenExp :: ASTExp -> Codegen [Instr]
codeGenExp (IntE i) = pure [Ldc i]
codeGenExp (BoolE True) = pure [Ldc 1]
codeGenExp (BoolE False) = pure [Ldc 0]
codeGenExp (NilE) = pure [Ldc 0]
codeGenExp (Var id []) = do
    loc <- lookupVar id
    pure [Ldl loc]
codeGenExp (Var id flds) = do
    let field = last flds
    let rest = init flds
    code <- codeGenExp (Var id rest)
    case field of
        Fst -> pure (code ++ [Ldh (-1)])
        Snd -> pure (code ++ [Ldh 0])
        Hd -> pure (code ++ [Ldh (-1)])
        Tl  -> pure (code ++ [Ldh 0])
codeGenExp (PairE e1 e2) = fmap mconcat $ sequence
    [ codeGenExp e1
    , codeGenExp e2
    , pure [Stmh 2]
    ]
codeGenExp (Op2E Cons e1 e2) = fmap mconcat $ sequence
    [ codeGenExp e1
    , codeGenExp e2
    , pure [Stmh 2]
    ]
codeGenExp (Op2E op e1 e2) = fmap mconcat $ sequence
    [ codeGenExp e1
    , codeGenExp e2
    , pure [opToInstr op]
    ]
codeGenExp (Op1E op e) = fmap mconcat $ sequence
    [ codeGenExp e
    , pure [op1ToInstr op]
    ]
codeGenExp (FunCallE id args) = do
    argCode <- fmap mconcat $ sequence (fmap codeGenExp args)
    pure (argCode ++ [Bsr id, Ajs (toInteger (-(length args))), LdrRR])


codeGenStmt :: String -> ASTStmt -> Codegen [Instr]
codeGenStmt _ (AssignS id [] e) = do
    exprCode <- codeGenExp e
    location <- lookupVar id
    pure (exprCode ++ [Stl location])
codeGenStmt _ (AssignS id flds e) = do
    let field = last flds
    let rest = init flds
    exprCode <- codeGenExp e
    varCode   <- codeGenExp (Var id rest)
    case field of
        Fst -> pure (exprCode ++ varCode ++ [Sta (-1)])
        Snd  -> pure (exprCode ++ varCode ++ [Sta 0])
        Hd -> pure (exprCode ++ varCode ++ [Sta (-1)])
        Tl -> pure (exprCode ++ varCode ++ [Sta 0])
codeGenStmt _ (FunCallS id args) = do
    argCode <- fmap mconcat $ sequence (fmap codeGenExp args)
    pure (argCode ++ [Bsr id, Ajs (toInteger (-(length args)))])
codeGenStmt retLabel (ReturnS (Just e)) = fmap mconcat $ sequence
    [ codeGenExp e
    , pure [StrRR, Bra retLabel]
    ]
codeGenStmt retLabel (ReturnS Nothing) = pure $ [Bra retLabel]
codeGenStmt retLabel (WhileS e body) = do
    cond <- codeGenExp e
    bodyCode <- fmap mconcat $ sequence (fmap (codeGenStmt retLabel) body)
    uni <- newUnique
    let beginL = "while" ++ show uni
    let endL = "whileend" ++ show uni
    pure   ([Label beginL]
        ++ cond
        ++ [Brf endL]
        ++ bodyCode
        ++ [Bra beginL, Label endL])
codeGenStmt retLabel (IfS e thenBody Nothing) = do
    cond <- codeGenExp e
    bodyCode <- fmap mconcat $ sequence (fmap (codeGenStmt retLabel) thenBody)
    uni <- newUnique
    let endL = "ifend" ++ show uni
    pure   (cond
        ++ [Brf endL]
        ++ bodyCode
        ++ [Label endL])
codeGenStmt retLabel (IfS e thenBody (Just elseBody)) = do
    cond <- codeGenExp e
    bodyCode <- fmap mconcat $ sequence (fmap (codeGenStmt retLabel) thenBody)
    elseCode <- fmap mconcat $ sequence (fmap (codeGenStmt retLabel) elseBody)
    uni <- newUnique
    let endL = "ifend" ++ show uni
    let elseL = "else" ++ show uni
    pure   (cond
        ++ [Brf elseL]
        ++ bodyCode
       ++ [Bra endL, Label elseL]
        ++ elseCode
        ++ [Label endL])

handleDecls :: [ASTVarDecl] -> Codegen Integer
handleDecls ((VarDecl _ id _):ds) = do
    rest <- handleDecls ds
    insertCtx (id, rest+1)
    pure (rest + 1)
handleDecls [] = pure 0

codeGenFunDecl :: ASTFunDecl -> Codegen [Instr]
codeGenFunDecl (FunDecl id args _ decls body) = do
    --Puts locals in the context and returns amount of space needed.
    ctx <- get
    if length args > 0 then
        mapM_ insertCtx
                (zip
                        (reverse args)
                        [(-(toInteger $ length args))-1 .. -2]
                )
    else
        pure ()
    space <- handleDecls decls
    prologue <- pure [Label id, Link space]
    init <- fmap mconcat $ sequence (fmap codeGenVarDecl decls)
    let retLabel = id ++ "XZXreturn"
    main  <- fmap mconcat $ sequence (fmap (codeGenStmt retLabel) body)
    epilouge <- pure [Label retLabel, Unlink, Ret]
    put ctx -- restore context
    pure $ mconcat [prologue, init, main, epilouge]

codeGenVarDecl :: ASTVarDecl -> Codegen [Instr]
codeGenVarDecl (VarDecl _ id e) = do
    expCode <- codeGenExp e
    loc <- lookupVar id
    pure $ expCode ++ [Stl loc]
    --Evaluate expression and assign var. (requires code)

codeGen :: AST -> Codegen [Instr]
codeGen ast = do
    c <- codeGen' ast
    pure ((Bsr "main"):Halt:c)

codeGen' :: AST -> Codegen [Instr]
codeGen' (AST (FunD f:rs)) = do
    funCode  <- codeGenFunDecl f
    restCode <- codeGen' (AST rs)
    pure (funCode ++ restCode)
codeGen' (AST (VarD _:rs)) = codeGen' (AST rs) --ignore global variables for now
codeGen' (AST []) = pure []


stupidPrint =
    [ Label "print"
    , Link 0
    , Ldl (0-2)
    , Trap 0
    , Unlink
    , Ret
    ]
