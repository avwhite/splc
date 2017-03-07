module Parser.Parser where

import Parser.Combinators
import Parser.AST

import Control.Applicative
import Control.Applicative.Alternative

import Data.Semigroup
import Data.List.NonEmpty

type SPLParser a = Parser Token a

tok :: Token -> a -> SPLParser a
tok t a = match t (== t) *> pure a

eat :: Token -> SPLParser ()
eat t = tok t ()

seof :: SPLParser ()
seof = eat EofTok

prnth p = eat LParTok *> p <* eat RParTok
brckt p = eat LBracketTok *> p <* eat RBracketTok
sqbrk p = eat LSqBracketTok *> p <* eat RSqBracketTok

idp :: SPLParser Identifier
idp = (\(IdTok i) -> i) <$> match (IdTok "") idp' where
    idp' (IdTok _) = True
    idp' _ = False

intLitp :: SPLParser Int
intLitp = (\(IntLitTok i) -> i) <$> match (IntLitTok 0) idp' where
    idp' (IntLitTok _) = True
    idp' _ = False

typep :: SPLParser ASTType
typep =
        (tok BoolTok BoolT)
    <|> (tok IntTok IntT)
    <|> (tok CharTok CharT)
    <|> ((uncurry PairT) <$> prnth ((,) <$> typep <*> (eat CommaTok *> typep)))
    <|> (ListT <$> sqbrk typep)
    <|> (PolyT <$> idp)

returnTypep :: SPLParser ASTReturnType
returnTypep = (tok VoidTok Void) <|> (ReturnType <$> typep)

funTypep :: SPLParser ASTFunType
funTypep = FunType <$> (many typep <* eat ArrowTok) <*> returnTypep

op1p :: SPLParser Op1
op1p = (tok MinusTok Neg) <|> (tok NotTok Not)

--Note: Using asum here might be dangerous if there are some precedence levels
--with no operators, since the Alternative identity contains undefined in case
--of error. Should not be a problem in practice.
op2p :: Precedence -> SPLParser Op2
op2p pr = foldl1 (<|>) [(tok t p) | (t,p) <- op2TokenAst, precedence p == pr] where
    op2TokenAst =
        [(EqTok, Equal), (NeTok, NotEq), (LtTok, Less)
        ,(LeTok, LessEq), (GtTok, Greater), (GeTok, GreaterEq)
        ,(ColonTok, Cons), (PlusTok, Plus), (MinusTok, Minus)
        ,(OrTok, Or), (TimesTok, Times), (DivTok, Div)
        ,(ModTok, Mod), (AndTok, And)]

fieldp :: SPLParser Field
fieldp = eat DotTok *> fieldName where
    fieldName =
            (tok HdTok Hd)
        <|> (tok TlTok Tl)
        <|> (tok FstTok Fst)
        <|> (tok SndTok Snd)

expp :: Precedence -> SPLParser ASTExp
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
    expp' :: Precedence -> SPLParser ASTExp
    expp' pr
        | pr /= highestPrecedence = expp (pr + 1)
        | otherwise =
                (tok FalseTok (BoolE False))
            <|> (tok TrueTok (BoolE True))
            <|> (tok EmptyListTok NilE)
            <|> Var <$> idp <*> many fieldp
            <|> FunCallE <$> idp <*> prnth (manySep (eat CommaTok) (expp 0))
            <|> (Op1E <$> op1p <*> expp' pr) -- unary ops binds tightest
            <|> (IntE <$> intLitp)
            <|> (eat LParTok *> expp 0 <* eat RParTok)
            <|> (PairE <$>
                    (eat LParTok *> (expp 0 <* eat CommaTok)) 
                <*> (expp 0 <* eat RParTok))

stmtp :: SPLParser ASTStmt
stmtp = ifp <|> whilep <|> assignp <|> funcallp <|> returnp where
    ifp = IfS
        <$> ((eat IfTok) *> prnth (expp 0))
        <*> brckt (many stmtp)
        <*> optional (eat ElseTok *> brckt (many stmtp))
    whilep = WhileS
        <$> (eat WhileTok *> prnth (expp 0))
        <*> brckt (many stmtp)
    assignp = AssignS
        <$> idp
        <*> (many fieldp)
        <*> (eat AssignTok *> expp 0 <* eat SemiColonTok) 
    funcallp = FunCallS
        <$> idp
        <*> (prnth (manySep (eat CommaTok) (expp 0)) <* eat SemiColonTok)
    returnp = ReturnS
        <$> (eat ReturnTok *> optional (expp 0) <* eat SemiColonTok)

varDeclp :: SPLParser ASTVarDecl
varDeclp = VarDecl
    <$> ((eat VarTok *> pure Nothing) <|> Just <$> typep)
    <*> idp
    <*> (eat AssignTok *> expp 0 <* eat SemiColonTok)

funDeclp :: SPLParser ASTFunDecl
funDeclp = FunDecl
    <$> idp
    <*> prnth (manySep (eat CommaTok) idp)
    <*> optional (eat OfTypeTok *> funTypep)
    <*> (eat LBracketTok *> many varDeclp)
    <*> (some stmtp <* eat RBracketTok)

declp :: SPLParser ASTDecl
declp = (VarD <$> varDeclp) <|> (FunD <$> funDeclp)

splp :: SPLParser AST
splp = some declp
