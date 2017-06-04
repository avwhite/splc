module Codegen.Codegen where

import Parser.AST

import Control.Monad.State

import Data.Traversable
import Data.Maybe
import Data.Char

data Instr = Halt
    | Ldc Integer | Ldl Integer | Lds Integer
    | Label String | Bsr String
    | Bra String | Brf String
    | Link Integer | Unlink | Ret | Stl Integer
    | StrRR | LdrRR | Ajs Integer
    | Ldh Integer | Stmh Integer | Sta Integer
    | Add | Mul | Sub | DivI | ModI | AndI | OrI
    | Eq | Ne | Lt | Gt | Le | Ge
    | Neg | Not
    | Trap Integer
    | Str String deriving (Show)

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

--detagCode False =
--    [ Lds 0
--    , Ldc 2
--    , ModI
--    , Codegen.Codegen.Not
--    , Ldc 3
--    , Add
--    , DivI]
detagCode False = [Ldc 2, DivI]
detagCode True = []

tagCode True = [Ldc 2, Mul]
tagCode False = []

--You should only pass False to this function when you KNOW you are
--asking for a stack value.
--If you need an untagged heap value, just ask for a tagged one, since
--it is the same anyways.
--If you need an untagged value and you don't know if it is heap or
--stack we have a problem, the more complicated and outcommented
--version of detagCode above should be use. But I don't think this
--will be neccesary.
codeGenExp :: Bool -> ASTExp -> Codegen [Instr]
codeGenExp tagged (IntE i) = pure ([Ldc i] ++ tagCode tagged)
codeGenExp tagged (CharE c) = pure ([Ldc (toInteger (ord c))] ++ tagCode tagged)
codeGenExp tagged (BoolE True) = pure ([Ldc 1] ++ tagCode tagged)
codeGenExp tagged (BoolE False) = pure ([Ldc 0] ++ tagCode tagged)
codeGenExp tagged (NilE) = pure ([Ldc 0] ++ tagCode tagged)
codeGenExp tagged (Var id []) = do
    loc <- lookupVar id
    pure ([Ldl loc] ++ detagCode tagged)
codeGenExp tagged (Var id flds) = do
    let field = last flds
    let rest = init flds
    --Here we ask for a tagged heap value even though we want an
    --untagged one. Since untagging a heap value is a nop and we no
    --that we are dealing with a heap value there is no reason.
    code <- codeGenExp True (Var id rest)
    case field of
        Fst -> pure (code ++ ([Ldh (-1)] ++ detagCode tagged))
        Snd -> pure (code ++ ([Ldh 0] ++ detagCode tagged))
        Hd -> pure (code ++ ([Ldh (-1)]  ++ detagCode tagged))
        Tl  -> pure (code ++ ([Ldh 0] ++ detagCode tagged))
codeGenExp tagged (PairE e1 e2) = fmap mconcat $ sequence
    [ codeGenExp True e1 -- We do not know if these are stack or heap
    , codeGenExp True e2 -- but fine since we want them tagged.
    , pure [Bsr "alloc", Ajs (-2), LdrRR] --Heap value, same when tagged as when untagged.
    ]
codeGenExp tagged (Op2E Cons e1 e2) = fmap mconcat $ sequence
    [ codeGenExp True e1
    , codeGenExp True e2
    , pure [Bsr "alloc", Ajs (-2), LdrRR] --Same as above
    ]
codeGenExp tagged (Op2E op e1 e2) = fmap mconcat $ sequence
    [ codeGenExp False e1
    , codeGenExp False e2
    , pure ([opToInstr op] ++ tagCode tagged)
    ]
codeGenExp tagged (Op1E op e) = fmap mconcat $ sequence
    [ codeGenExp False e
    , pure ([op1ToInstr op] ++ tagCode tagged)
    ]
codeGenExp tagged (FunCallE id args) = do
    --Args should be tagged since they will be loaded as variables.
    argCode <- fmap mconcat $ sequence (fmap (codeGenExp True) args)
    pure (argCode ++ [Bsr id, Ajs (toInteger (-(length args))), LdrRR] ++ detagCode tagged)

codeGenStmt :: String -> ASTStmt -> Codegen [Instr]
codeGenStmt _ (AssignS id [] e) = do
    exprCode <- codeGenExp True e
    location <- lookupVar id
    pure (exprCode ++ [Stl location])
codeGenStmt _ (AssignS id flds e) = do
    let field = last flds
    let rest = init flds
    exprCode <- codeGenExp True e
    --We want an untagged value, but we ask for a tagged one because
    --we know we are dealing with a heap value and it does not matter.
    varCode   <- codeGenExp True (Var id rest)
    case field of
        Fst -> pure (exprCode ++ varCode ++ [Sta (-1)])
        Snd  -> pure (exprCode ++ varCode ++ [Sta 0])
        Hd -> pure (exprCode ++ varCode ++ [Sta (-1)])
        Tl -> pure (exprCode ++ varCode ++ [Sta 0])
codeGenStmt _ (FunCallS id args) = do
    argCode <- fmap mconcat $ sequence (fmap (codeGenExp True) args)
    pure (argCode ++ [Bsr id, Ajs (toInteger (-(length args)))])
codeGenStmt retLabel (ReturnS (Just e)) = fmap mconcat $ sequence
    [ codeGenExp True e
    , pure [StrRR, Bra retLabel]
    ]
codeGenStmt retLabel (ReturnS Nothing) = pure $ [Bra retLabel]
codeGenStmt retLabel (WhileS e body) = do
    cond <- codeGenExp False e
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
    cond <- codeGenExp False e
    bodyCode <- fmap mconcat $ sequence (fmap (codeGenStmt retLabel) thenBody)
    uni <- newUnique
    let endL = "ifend" ++ show uni
    pure   (cond
        ++ [Brf endL]
        ++ bodyCode
        ++ [Label endL])
codeGenStmt retLabel (IfS e thenBody (Just elseBody)) = do
    cond <- codeGenExp False e
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
handleDecls [] = pure 1

codeGenFunDecl :: ASTFunDecl -> Codegen [Instr]
codeGenFunDecl (FunDecl id args _ decls body) = do
    --Puts locals in the context and returns amount of space needed.
    (ctx,_) <- get
    if length args > 0 then
        mapM_ insertCtx
                (zip
                        (reverse args)
                        [(-(toInteger $ length args))-1 .. -2]
                )
    else
        pure ()
    space <- handleDecls decls
    prologue <- pure [Label id, Link (space + 1), Ldc space, Stl 1]
    init <- fmap mconcat $ sequence (fmap codeGenVarDecl decls)
    let retLabel = id ++ "XZXreturn"
    main  <- fmap mconcat $ sequence (fmap (codeGenStmt retLabel) body)
    epilouge <- pure [Label retLabel, Unlink, Ret]
    (_,i) <- get
    put (ctx,i)
    pure $ mconcat [prologue, init, main, epilouge]

codeGenVarDecl :: ASTVarDecl -> Codegen [Instr]
codeGenVarDecl (VarDecl _ id e) = do
    expCode <- codeGenExp True e
    loc <- lookupVar id
    pure $ expCode ++ [Stl loc]
    --Evaluate expression and assign var. (requires code)

heapSpace = 16

codeGen :: AST -> Codegen [Instr]
codeGen ast = do
    c <- codeGen' ast
    pure $ [ Str "Ldr HP"
           , Ldc heapSpace
           , Add
           , Lds 0
           , Str "Str R5"
           , Str "Str R6"
           , Str "Ldr MP"
           , Str "Str R7"
           , Bsr "main"
           , Halt]
        ++ c
        ++ allocCode
        ++ isEmptyCode
        ++ readCode
        ++ stupidPrint

codeGen' :: AST -> Codegen [Instr]
codeGen' (AST (FunD f:rs)) = do
    funCode  <- codeGenFunDecl f
    restCode <- codeGen' (AST rs)
    pure (funCode ++ restCode)
codeGen' (AST (VarD _:rs)) = codeGen' (AST rs) --ignore global variables for now
codeGen' (AST []) = pure []


forwardCode =
    [ Label "forward"
    , Str "Ldr R5"
    , Ldc heapSpace
    , Sub
    , Ldl (-2)
    , Le
    , Ldc heapSpace
    , Str "Ldr R5"
    , Lt
    , AndI
    , Brf "forwardlabel1"
    , Str "Ldr R5"
    , Ldc heapSpace
    , Sub
    , Ldl (-2)
    , Ldh (-2)
    , Le
    , Ldc heapSpace
    , Str "Ldr R5"
    , Lt
    , AndI
    , Brf "forwardlabel2"
    , Ldl (-2) --Load struct content
    , Ldh (-1)
    , Ldl (-2)
    , Ldh 0
    , Bsr "alloc" --move it
    , Ajs (-2)
    , LdrRR
    , Ldl (-2)
    , Sta (-3)
    , Bra "forwardend"
    , Label "forwardlabel2" -- in this case we just return the fp
    , Ldl (-2)
    , Ldh (-3)
    , StrRR
    , Bra "forwardend"
    , Label "forwardlabel1" -- in this case we return the pointer itself
    , Ldl (-2)
    , StrRR
    , Label "forwardend"
    , Unlink
    , Ret]


collectCode =
    [ Label "collect"
 ]

allocCode =
    [ Label "alloc"
    , Link 1
    , Ldc 0
    , Stl 1
    , Str "Ldr R5"
    , Ldc 4
    , Sub
    , Str "Ldr HP"
    , Lt
    , Brf "skipgc"
    , Bsr "collect"
    , Label "skipgc"
    , Ldc 0
    , Str "Ldr HP" -- Forwarding pointer needs to point in current space
    , Ldl (-3)
    , Ldl (-2)
    , Stmh 4
    , StrRR
    , Unlink
    , Ret
    ]

isEmptyCode =
    [ Label "isEmpty"
    , Link 1
    , Ldc 0
    , Stl 1
    , Ldl (0-2)
    , Ldc 0
    , Eq
    , Ldc 2
    , Mul --We need to tag bool before returning it.
    , StrRR
    , Unlink
    , Ret
    ]

readCode =
    [ Label "read"
    , Link 0
    , Trap 12
    , Ldc 0
    , Label "readloopXZX"
    , Stmh 2
    , Lds (-1)
    , Ldc 0
    , Eq
    , Brf "readloopXZX"
    , StrRR
    , Unlink
    , Ret
    ]

stupidPrint =
    [ Label "print"
    , Link 0
    , Ldl (0-2)
    , Trap 0
    , Unlink
    , Ret
    ]
