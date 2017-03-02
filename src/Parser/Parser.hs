module Parser.Parser where

import Scanner
import Parser.Combinators
import Parser.AST

import Control.Applicative
import Control.Applicative.Alternative

parse :: (Parser Token a) -> String -> a
parse p s = (fst . head) (runParser (p <* eof) (alexScanTokens s))

prnth p = eat LParTok *> p <* eat RParTok
brckt p = eat LBracketTok *> p <* eat RBracketTok
sqbrk p = eat LSqBracketTok *> p <* eat RSqBracketTok

identName (IdTok i) = i

typep :: Parser Token ASTType
typep =
        (eat BoolTok *> pure BoolT)
    <|> (eat IntTok *> pure IntT)
    <|> (eat CharTok *> pure CharT)
    <|> (PairT <$> 
                (eat LParTok *> typep)
            <*> (eat CommaTok *> typep))
            <*  eat RParTok
    <|> (ListT <$> (eat LSqBracketTok *> typep <* eat RSqBracketTok))
    <|> ((PolyT . (\(IdTok i) -> i)) <$> (eat (IdTok "does_not_matter")))

returnTypep :: Parser Token ASTReturnType
returnTypep = (eat VoidTok *> pure Void)  <|> (ReturnType <$> typep)

funTypep :: Parser Token ASTFunType
funTypep = FunType <$> (many typep <* eat ArrowTok) <*> returnTypep

op1p :: Parser Token Op1
op1p = (tok MinusTok Neg) <|> (tok NotTok Not)

op2p :: Precedence -> Parser Token Op2
op2p pr = asum [(tok t p) | (t,p) <- op2TokenAst, precedence p == pr] where
    op2TokenAst =
        [(EqTok, Equal), (NeTok, NotEq), (LtTok, Less)
        ,(LeTok, LessEq), (GtTok, Greater), (GeTok, GreaterEq)
        ,(ColonTok, Cons), (PlusTok, Plus), (MinusTok, Minus)
        ,(OrTok, Or), (TimesTok, Times), (DivTok, Div)
        ,(ModTok, Mod), (AndTok, And)]

fieldp :: Parser Token Field
fieldp = eat DotTok *> fieldName where
    fieldName =
            (tok HdTok Hd)
        <|> (tok TlTok Tl)
        <|> (tok FstTok Fst)
        <|> (tok SndTok Snd)

expp :: Precedence -> Parser Token ASTExp
expp pr = makeExpAst (assoc pr) <$> someSep' (op2p pr) (expp' pr) where

    rightifyExpList :: (b,[(a,b)]) -> ([(b,a)],b)
    rightifyExpList (b,[]) = ([], b)
    rightifyExpList (b,(a,c):t) =
        (\(l,e) -> ((b,a):l, e)) (rightifyExpList (c,t))

    makeExpAst :: Associativity -> (ASTExp, [(Op2, ASTExp)]) -> ASTExp
    makeExpAst LeftAssoc (x,xs) = foldl (\e1 (o, e2) -> Op2E o e1 e2) x xs
    makeExpAst RightAssoc l = makeExpAst' (rightifyExpList l) where
        makeExpAst' (xs,x) = foldr (\(e1, o) e2 -> Op2E o e1 e2) x xs

    --TODO: What about Char?
    expp' :: Precedence -> Parser Token ASTExp
    expp' pr
        | pr /= highestPrecedence = expp (pr + 1)
        | otherwise =
                (tok FalseTok (BoolE False))
            <|> (tok TrueTok (BoolE True))
            <|> (tok EmptyListTok NilE)
            <|> ((\(IdTok i) -> Var i) <$> eat (IdTok "") <*> many fieldp)
            <|> ((\(IdTok i) -> FunCallE i) <$> 
                    eat (IdTok "")
                <*> (eat LParTok *> 
                    manySep (eat CommaTok) (expp 0)
                    <* eat RParTok))
            <|> (Op1E <$> op1p <*> expp' pr) -- unary ops binds tightest
            <|> ((\(IntLitTok i) -> IntE i) <$> eat (IntLitTok 0))
            <|> (eat LParTok *> expp 0 <* eat RParTok)
            <|> (PairE <$>
                    (eat LParTok *> (expp 0 <* eat CommaTok)) 
                <*> (expp 0 <* eat RParTok))

stmtp :: Parser Token ASTStmt
stmtp = ifp <|> whilep <|> assignp <|> funcallp <|> returnp where
    ifp = IfS
        <$> ((eat IfTok) *> prnth (expp 0))
        <*> brckt (many stmtp)
        <*> optional (eat ElseTok *> brckt (many stmtp))
    whilep = WhileS
        <$> (eat WhileTok *> prnth (expp 0))
        <*> brckt (many stmtp)
    assignp = AssignS . identName 
        <$> (eat (IdTok ""))
        <*> (many fieldp)
        <*> (eat AssignTok *> expp 0 <* eat SemiColonTok) 
    funcallp = FunCallS . identName
        <$> eat (IdTok "")
        <*> (prnth (manySep (eat CommaTok) (expp 0)) <* eat SemiColonTok)
    returnp = ReturnS
        <$> (eat ReturnTok *> optional (expp 0) <* eat SemiColonTok)

varDeclp :: Parser Token ASTVarDecl
varDeclp = VarDecl
    <$> ((eat VarTok *> pure Nothing) <|> Just <$> typep)
    <*> (identName <$> eat (IdTok ""))
    <*> (eat AssignTok *> expp 0 <* eat SemiColonTok)

funDeclp :: Parser Token ASTFunDecl
funDeclp = FunDecl
    <$> (identName <$> eat (IdTok ""))
    <*> prnth (manySep (eat CommaTok) (identName <$> eat (IdTok "")))
    <*> optional (eat OfTypeTok *> funTypep)
    <*> (eat LBracketTok *> many varDeclp)
    <*> (some stmtp <* eat RBracketTok)

declp :: Parser Token ASTDecl
declp = (VarD <$> varDeclp) <|> (FunD <$> funDeclp)

splp :: Parser Token AST
splp = some declp
