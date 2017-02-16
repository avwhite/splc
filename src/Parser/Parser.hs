module Parser.Parser where

import Scanner
import Parser.Combinators

import Control.Applicative

--Very simple test
type Identifier = String
data ASTType = BoolT | IntT | CharT | PairT ASTType ASTType | ListT ASTType
    | PolyT Identifier deriving Show
data ASTReturnType = Void | ReturnType ASTType deriving Show
data ASTFunType = FunType [ASTType] ASTReturnType deriving Show

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
