module Parser.Parser where

import Scanner
import Parser.Combinators
import Parser.AST

import Control.Applicative
import Control.Applicative.Alternative

parse :: (Parser Token a) -> String -> a
parse p s = (fst . head) (runParser (p <* eof) (alexScanTokens s))

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

--TODO: Associativity. For now operators on the same precedence level always
--      associate to the right
expp :: Precedence -> Parser Token ASTExp
expp pr = combine <$> expp' pr <*> expp'' pr where
    combine e1 Nothing = e1
    combine e1 (Just (o, e2)) = Op2E o e1 e2

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

    expp'' :: Precedence -> Parser Token (Maybe (Op2, ASTExp))
    expp'' pr = optional ((,) <$> op2p pr <*> expp pr)
