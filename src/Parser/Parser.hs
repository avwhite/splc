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

tok :: (Eq a) => a -> b -> Parser a b
tok a b = (eat a *> pure b)

op2p :: Parser Token Op2
op2p =
        (tok PlusTok Plus)  <|> (tok MinusTok Minus) <|> (tok TimesTok Times)
    <|> (tok DivTok Div)    <|> (tok ModTok Mod)
    <|> (tok EqTok Equal)   <|> (tok NeTok NotEq)
    <|> (tok LtTok Less)    <|> (tok LeTok LessEq)
    <|> (tok GtTok Greater) <|> (tok GeTok GreaterEq)
    <|> (tok AndTok And)    <|> (tok OrTok Or)
    <|> (tok ColonTok Cons)

--TODO: Most forms of expressions
--TODO: Precedence of operators. For now everything associates to the right.
expp :: Parser Token ASTExp
expp = combine <$> expp' <*> expp'' where
    combine e1 Nothing = e1
    combine e1 (Just (o, e2)) = Op2E o e1 e2

    --TODO: What about Char?
    expp' :: Parser Token ASTExp
    expp' =
            (tok FalseTok (BoolE False))
        <|> (tok TrueTok (BoolE True))
        <|> ((\(IntLitTok i) -> IntE i) <$> eat (IntLitTok 0))
        <|> (eat LParTok *> expp <* eat RParTok)

    expp'' :: Parser Token (Maybe (Op2, ASTExp))
    expp'' = optional ((,) <$> op2p <*> expp)
