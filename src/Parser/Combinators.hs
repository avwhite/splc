module Parser.Combinators where

import Scanner
import Control.Monad
import Control.Applicative
import Data.Data
import Data.List

data Parser t a = Parser ([t] -> [(a, [t])])

runParser :: Parser t a -> [t] -> [(a,[t])]
runParser (Parser p) = p

instance Functor (Parser t) where
    fmap = liftM

instance Applicative (Parser t) where
    pure a = Parser (\ts -> [(a, ts)])
    (<*>) = ap

instance Monad (Parser t) where
    (>>=) p f = Parser (\ts -> do
        (a, ts2) <- runParser p ts
        (b, ts3) <- runParser (f a) ts2
        pure (b, ts3)
        )

instance Alternative (Parser t) where
    empty = Parser (\ts -> [])
    (<|>) p1 p2 = Parser (\ts -> runParser p1 ts ++ runParser p2 ts)

--Eager version of alternative. Dosen't consider p2 of p1 is a success.
infixl 3 <<|>
(<<|>) :: Parser t s -> Parser t s -> Parser t s
(<<|>) p1 p2 = Parser (\ts -> let l = runParser p1 ts
    in if null l then runParser p2 ts else l) 

--Eager version of many. Only parses the longest possible list. Maby will be
--usefull for unterminated lists?
eagerMany :: Parser t a -> Parser t [a]
eagerMany p = ((:) <$> p <*> eagerMany p) <<|> (pure [])

--Eager version of some. Only parses the longest possible list. Maby will be
--usefull for unterminated lists?
eagerSome :: Parser t a -> Parser t [a]
eagerSome p = (:) <$> p <*> eagerMany p

eat :: Token -> Parser Token Token
eat match = Parser eat' where
    eat' [] = []
    eat' (t:ts)
        | t == match = [(t,ts)]
        | otherwise = []
