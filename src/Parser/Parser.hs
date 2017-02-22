module Parser.Parser where

import Scanner
import Parser.Combinators
import Parser.AST

import Control.Applicative

tok :: (Eq a) => a -> b -> Parser a b
tok a b = (eat a *> pure b)

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

--TODO: Most forms of expressions
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
            <|> ((\(IntLitTok i) -> IntE i) <$> eat (IntLitTok 0))
            <|> (eat LParTok *> expp 0 <* eat RParTok)

    expp'' :: Precedence -> Parser Token (Maybe (Op2, ASTExp))
    expp'' pr = optional ((,) <$> op2p pr <*> expp pr)
