module Parser where

import Scanner

data ParseError t = Expected t

data Parser t a = Parser ([t] -> (Either (ParseError t) a, [t]))
type TokenParser = Parser Token

runParser :: Parser t a -> [t] -> (Either (ParseError t) a,[t])
runParser (Parser p) = p

instance Functor (Parser t) where
    fmap f (Parser p) = Parser (\ts -> case p ts of
        (Left err, ts2) -> (Left err, ts2)
        (Right a, ts2) -> (Right (f a), ts2))

instance Applicative (Parser t) where
    pure a = Parser (\ts -> (Right a, ts))
    (<*>) (Parser pf) a = Parser (\ts -> case pf ts of
        (Left err, ts2) -> (Left err, ts2)
        (Right f, ts2) -> runParser (fmap f a) ts2)

instance Monad (Parser t) where
    (>>=) (Parser pa) f = Parser (\ts -> case pa ts of
        (Left err, ts2) -> (Left err, ts2)
        (Right a, ts2) -> runParser (f a) ts2)

peek :: Int -> Parser Token [Token]
peek n = Parser (\ts -> (Right (take n ts), ts))

eat :: Token -> Parser Token Token
eat match = Parser eat' where
    eat' :: [Token] -> (Either (ParseError Token) Token, [Token])
    eat' [] = (Left (Expected match), [])
    eat' (t:ts) = if (t == match) then 
        (Right t, ts) else
        (Left (Expected match), ts)

--Very simple test
data ASTType = BoolT | IntT | CharT | PairT ASTType ASTType | ListT ASTType
    -- | PolyT Identifier

ruleType :: Parser Token ASTType
ruleType = do
    lah <- peek 1
    case lah of
        --First three are the rule BasicType inlined here.
        [BoolTok] -> eat BoolTok >> pure BoolT
        [IntTok] -> eat IntTok >> pure IntT
        [CharTok] -> eat CharTok >> pure CharT
        [LParTok] -> PairT <$> 
            (eat LParTok >> ruleType) <*> (eat CommaTok >> ruleType)
        [LSqBracketTok] -> ListT <$> (eat LSqBracketTok >> ruleType)
     --   [IdTok] -> PolyT <$> ruleIdentifier
