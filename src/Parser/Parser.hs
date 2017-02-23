module Parser.Parser where

import Scanner
import Parser.Combinators
import Parser.AST

import Control.Applicative

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

type Precedence = Int

op2p :: Precedence -> Parser Token Op2
op2p 0 =  (tok EqTok Equal) <|> (tok NeTok NotEq)
op2p 1 =
        (tok LtTok Less)
    <|> (tok LeTok LessEq)
    <|> (tok GtTok Greater)
    <|> (tok GeTok GreaterEq)
op2p 2 = (tok ColonTok Cons)
op2p 3 = (tok PlusTok Plus)  <|> (tok MinusTok Minus) <|> (tok OrTok Or)
op2p 4 =
        (tok TimesTok Times)
    <|> (tok DivTok Div)
    <|> (tok ModTok Mod)
    <|> (tok AndTok And)

highestPrecedence = 4 :: Int

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
