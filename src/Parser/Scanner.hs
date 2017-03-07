module Parser.Scanner where

import Parser.AST
import Parser.Combinators
import Control.Applicative
import Data.Char
import Data.Maybe

type Scanner = Parser Char [Token]

scan str = parse (scanner <* eof) str

eat :: String -> Parser Char String
eat cs = foldr (liftA2 (:)) (pure []) $ (fmap one cs) where
    one c = match c (== c)

tok :: String -> Token -> Parser Char Token
tok str tok = eat str *> pure tok

scanner = catMaybes <$> (many $ (whitespace <|> symbol <|> intLit <|> ident)) where
    whitespace = ((eager . some) $ match ' ' isSpace) *> pure Nothing
    ident = (Just . wordToToken ) <$> 
        ((:) 
            <$> (match 'a' isAlpha)
            <*> ((eager . many) $ match 'a' isAlphaNum))
    intLit = (Just . IntLitTok . read) <$> ((eager . some) $ match '0' isDigit)
    symbol = Just <$> (eager $ 
            (tok "==" EqTok)
        <|> (tok "<=" LeTok)
        <|> (tok ">=" GeTok)
        <|> (tok "!=" NeTok)
        <|> (tok "&&" AndTok)
        <|> (tok "||" OrTok)
        <|> (tok "=" AssignTok)
        <|> (tok ";" SemiColonTok)
        <|> (tok "(" LParTok)
        <|> (tok ")" RParTok)
        <|> (tok "::" OfTypeTok)
        <|> (tok "{" LBracketTok)
        <|> (tok "}" RBracketTok)
        <|> (tok "->" ArrowTok)
        <|> (tok "," CommaTok)
        <|> (tok "[" LSqBracketTok)
        <|> (tok "]" RSqBracketTok)
        <|> (tok "[]" EmptyListTok)
        <|> (tok "." DotTok)
        <|> (tok "+" PlusTok)
        <|> (tok "-" MinusTok)
        <|> (tok "*" TimesTok)
        <|> (tok "/" DivTok)
        <|> (tok "%" ModTok)
        <|> (tok "<" LtTok)
        <|> (tok ">" GtTok)
        <|> (tok ":" ColonTok)
        <|> (tok "!" NotTok))

wordToToken :: String -> Token
wordToToken "var" = VarTok
wordToToken "Void" = VoidTok
wordToToken "Int" = IntTok
wordToToken "Bool" = BoolTok
wordToToken "Char" = CharTok
wordToToken "if" = IfTok
wordToToken "else" = ElseTok
wordToToken "while" = WhileTok
wordToToken "return" = ReturnTok
wordToToken "False" = FalseTok
wordToToken "True" = TrueTok
wordToToken "hd" = HdTok
wordToToken "tl" = TlTok
wordToToken "fst" = FstTok
wordToToken "snd" = SndTok
wordToToken id = IdTok id
