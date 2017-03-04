module Parser.Parser where

import Scanner
import Parser.Combinators
import Parser.AST

import Control.Applicative
import Control.Applicative.Alternative

import Data.Semigroup
import Data.List.NonEmpty
import qualified Data.Set as Set

type EnumToken = (Integer, Token)
data ParseError = Expected
    (Set.Set (Maybe Token))
    (Maybe EnumToken) deriving (Show)

--Nothing represents EOF
further :: Maybe EnumToken -> Maybe EnumToken -> Bool
further Nothing Nothing = False
further Nothing _ = True
further (Just _) Nothing = False
further (Just (i1, _)) (Just (i2, _)) = i1 > i2

instance Semigroup ParseError where
    (<>) (Expected s1 t1) (Expected s2 t2) 
        | further t1 t2 = Expected s1 t1
        | further t2 t1 = Expected s2 t2
        | otherwise = Expected (s1 <> s2) t1

type SPLParser a = Parser (Integer, Token) ParseError a

tokMatch t1 (_, t2) = t1 == t2

tok :: Token -> a -> SPLParser a
tok t a = match
    (Expected (Set.singleton (Just t)) Nothing)
    (\found -> Expected (Set.singleton (Just t)) (Just found)) 
    (tokMatch t) *> pure a

eat :: Token -> SPLParser ()
eat t = tok t ()

seof :: SPLParser ()
seof = eof (\found -> Expected (Set.singleton Nothing) (Just found))

prnth p = eat LParTok *> p <* eat RParTok
brckt p = eat LBracketTok *> p <* eat RBracketTok
sqbrk p = eat LSqBracketTok *> p <* eat RSqBracketTok

identName (IdTok i) = i

idp :: SPLParser Identifier
idp = identName . snd <$> match 
    (Expected (Set.singleton (Just $ IdTok "")) Nothing)
    (\found -> Expected (Set.singleton (Just $ IdTok "")) (Just found))
    idp' where
        idp' (_, (IdTok _)) = True
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

--op1p :: Parser Token Op1
--op1p = (tok MinusTok Neg) <|> (tok NotTok Not)
--
--op2p :: Precedence -> Parser Token Op2
--op2p pr = asum [(tok t p) | (t,p) <- op2TokenAst, precedence p == pr] where
--    op2TokenAst =
--        [(EqTok, Equal), (NeTok, NotEq), (LtTok, Less)
--        ,(LeTok, LessEq), (GtTok, Greater), (GeTok, GreaterEq)
--        ,(ColonTok, Cons), (PlusTok, Plus), (MinusTok, Minus)
--        ,(OrTok, Or), (TimesTok, Times), (DivTok, Div)
--        ,(ModTok, Mod), (AndTok, And)]
--
--fieldp :: Parser Token Field
--fieldp = eat DotTok *> fieldName where
--    fieldName =
--            (tok HdTok Hd)
--        <|> (tok TlTok Tl)
--        <|> (tok FstTok Fst)
--        <|> (tok SndTok Snd)
--
--expp :: Precedence -> Parser Token ASTExp
--expp pr = makeExpAst (assoc pr) <$> someSep' (op2p pr) (expp' pr) where
--
--    rightifyExpList :: (b,[(a,b)]) -> ([(b,a)],b)
--    rightifyExpList (b,[]) = ([], b)
--    rightifyExpList (b,(a,c):t) =
--        (\(l,e) -> ((b,a):l, e)) (rightifyExpList (c,t))
--
--    makeExpAst :: Associativity -> (ASTExp, [(Op2, ASTExp)]) -> ASTExp
--    makeExpAst LeftAssoc (x,xs) = foldl (\e1 (o, e2) -> Op2E o e1 e2) x xs
--    makeExpAst RightAssoc l = makeExpAst' (rightifyExpList l) where
--        makeExpAst' (xs,x) = foldr (\(e1, o) e2 -> Op2E o e1 e2) x xs
--
--    --TODO: What about Char?
--    expp' :: Precedence -> Parser Token ASTExp
--    expp' pr
--        | pr /= highestPrecedence = expp (pr + 1)
--        | otherwise =
--                (tok FalseTok (BoolE False))
--            <|> (tok TrueTok (BoolE True))
--            <|> (tok EmptyListTok NilE)
--            <|> ((\(IdTok i) -> Var i) <$> eat (IdTok "") <*> many fieldp)
--            <|> ((\(IdTok i) -> FunCallE i) <$> 
--                    eat (IdTok "")
--                <*> (eat LParTok *> 
--                    manySep (eat CommaTok) (expp 0)
--                    <* eat RParTok))
--            <|> (Op1E <$> op1p <*> expp' pr) -- unary ops binds tightest
--            <|> ((\(IntLitTok i) -> IntE i) <$> eat (IntLitTok 0))
--            <|> (eat LParTok *> expp 0 <* eat RParTok)
--            <|> (PairE <$>
--                    (eat LParTok *> (expp 0 <* eat CommaTok)) 
--                <*> (expp 0 <* eat RParTok))
--
--stmtp :: Parser Token ASTStmt
--stmtp = ifp <|> whilep <|> assignp <|> funcallp <|> returnp where
--    ifp = IfS
--        <$> ((eat IfTok) *> prnth (expp 0))
--        <*> brckt (many stmtp)
--        <*> optional (eat ElseTok *> brckt (many stmtp))
--    whilep = WhileS
--        <$> (eat WhileTok *> prnth (expp 0))
--        <*> brckt (many stmtp)
--    assignp = AssignS . identName 
--        <$> (eat (IdTok ""))
--        <*> (many fieldp)
--        <*> (eat AssignTok *> expp 0 <* eat SemiColonTok) 
--    funcallp = FunCallS . identName
--        <$> eat (IdTok "")
--        <*> (prnth (manySep (eat CommaTok) (expp 0)) <* eat SemiColonTok)
--    returnp = ReturnS
--        <$> (eat ReturnTok *> optional (expp 0) <* eat SemiColonTok)
--
--varDeclp :: Parser Token ASTVarDecl
--varDeclp = VarDecl
--    <$> ((eat VarTok *> pure Nothing) <|> Just <$> typep)
--    <*> (identName <$> eat (IdTok ""))
--    <*> (eat AssignTok *> expp 0 <* eat SemiColonTok)
--
--funDeclp :: Parser Token ASTFunDecl
--funDeclp = FunDecl
--    <$> (identName <$> eat (IdTok ""))
--    <*> prnth (manySep (eat CommaTok) (identName <$> eat (IdTok "")))
--    <*> optional (eat OfTypeTok *> funTypep)
--    <*> (eat LBracketTok *> many varDeclp)
--    <*> (some stmtp <* eat RBracketTok)
--
--declp :: Parser Token ASTDecl
--declp = (VarD <$> varDeclp) <|> (FunD <$> funDeclp)
--
--splp :: Parser Token AST
--splp = some declp
