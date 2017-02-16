module Parser.Parser where

import Scanner
import Parser.Combinators
import Parser.AST

import Control.Applicative

parseSpl :: String -> ASTFunType
parseSpl s = (fst . head) (runParser funTypep (alexScanTokens s))

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
