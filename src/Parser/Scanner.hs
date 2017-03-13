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

scanner = catMaybes <$> (eagerMany $ ((blockComment <|> lineComment) <<|> whitespace <<|> symbol <<|> intLit <<|> ident)) where
    lineComment =
           eat "//"
        *> many (match 'a' (/= '\n'))
        *> match 'n' (== '\n')
        *> pure Nothing
    blockComment = eat "/*" *> blockComment' *> pure Nothing
    blockComment' =
           many (match 'a' (/= '*'))
        *> eat "*"
        *> (eat "/" <<|> blockComment')
    whitespace = ((eagerSome) $ match ' ' isSpace) *> pure Nothing
    ident = (Just . wordToToken ) <$> 
        ((:) 
            <$> (match 'a' isAlpha)
            <*> ((eagerMany) $ match 'a' (\c -> isAlphaNum c || c == '_')))
    intLit = (Just . IntLitTok . read) <$> ((eager . some) $ match '0' isDigit)
    symbol = Just <$> (
            (tok "==" EqTok)
        <<|> (tok "<=" LeTok)
        <<|> (tok ">=" GeTok)
        <<|> (tok "!=" NeTok)
        <<|> (tok "&&" AndTok)
        <<|> (tok "||" OrTok)
        <<|> (tok "[]" EmptyListTok)
        <<|> (tok "=" AssignTok)
        <<|> (tok ";" SemiColonTok)
        <<|> (tok "(" LParTok)
        <<|> (tok ")" RParTok)
        <<|> (tok "::" OfTypeTok)
        <<|> (tok "{" LBracketTok)
        <<|> (tok "}" RBracketTok)
        <<|> (tok "->" ArrowTok)
        <<|> (tok "," CommaTok)
        <<|> (tok "[" LSqBracketTok)
        <<|> (tok "]" RSqBracketTok)
        <<|> (tok "." DotTok)
        <<|> (tok "+" PlusTok)
        <<|> (tok "-" MinusTok)
        <<|> (tok "*" TimesTok)
        <<|> (tok "/" DivTok)
        <<|> (tok "%" ModTok)
        <<|> (tok "<" LtTok)
        <<|> (tok ">" GtTok)
        <<|> (tok ":" ColonTok)
        <<|> (tok "!" NotTok))

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
