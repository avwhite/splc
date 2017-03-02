module Parser.Combinators where

import Scanner
import Control.Monad
import Control.Applicative
import Data.Semigroup
import Data.List.NonEmpty

type ParseResult t e a = Either e (NonEmpty (a, [t]))
data Parser t e a = Parser ([t] -> ParseResult t e a)

runParser :: Parser t e a -> [t] -> ParseResult t e a
runParser (Parser p) = p

combineParses :: 
    (Semigroup e) => 
    ParseResult t e a -> 
    ParseResult t e a -> 
    ParseResult t e a
combineParses (Left _) (Right l) = Right l
combineParses (Right l) (Left _) = Right l
combineParses (Left m1) (Left m2) = Left (m1 <> m2)
combineParses (Right l1) (Right l2) = Right (l1 <> l2)

--technically I think we could provide a functor instance which does not
--require the Semigroup constraint on e, but this is easier.
instance (Semigroup e) => Functor (Parser t e) where
    fmap = liftM

instance (Semigroup e) => Applicative (Parser t e) where
    pure a = Parser (\ts -> Right ((a, ts) :| []))
    (<*>) = ap

instance (Semigroup e) => Monad (Parser t e) where
    (>>=) p f = Parser (\ts -> do
        res1 <- runParser p ts
        foldl1 combineParses $ fmap (\(a, ts2) -> runParser (f a) ts2) res1)

instance (Semigroup e) => Alternative (Parser t e) where
    empty = Parser (\ts -> Left undefined)
    (<|>) p1 p2 = Parser 
        (\ts -> combineParses (runParser p1 ts) (runParser p2 ts))

someSep :: (Alternative f) => f a -> f b -> f [b]
someSep sep x = (:) <$> x <*> many (sep *> x)

manySep :: (Alternative f) => f a -> f b -> f [b]
manySep sep x = someSep sep x <|> pure []

someSep' :: (Alternative f) => f a -> f b -> f (b,[(a,b)])
someSep' sep x = (,) <$> x <*> many ((,) <$> sep <*> x)

--manySep' not included because it is not needed for now, and the return type
--would end up being something horrendous like (Maybe (f (a,[(b,a)])))

eager :: Parser t e a -> Parser t e a
eager p = Parser e where
    e ts = case runParser p ts of
        Left e -> Left e
        Right (x :| t) -> Right (x :| [])

match :: e -> (t -> e) -> (t -> Bool) -> Parser t e t
match emptyError noMatchError pred = Parser match' where
    match' [] = Left emptyError
    match' (t:ts)
        | pred t = Right ((t,ts) :| [])
        | otherwise = Left (noMatchError t)

eof :: (t -> e) -> Parser t e ()
eof notEmptyError = Parser eof' where
    eof' [] = Right (((), []) :| [])
    eof' (t:ts) = Left (notEmptyError t)
