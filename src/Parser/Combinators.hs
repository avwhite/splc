module Parser.Combinators where

import Scanner
import Control.Monad
import Control.Applicative
import Data.Semigroup
import Data.List.NonEmpty
import qualified Data.Set as Set

type ParseSuccesses t a = NonEmpty (a, [(Integer, t)])
type ParseError t = ((Set.Set t), (Integer, t))

data ParseResult t a = 
      Success (ParseSuccesses t a)
    | Error (ParseError t)
    | Uncertain (ParseError t) (ParseSuccesses t a)

data Parser t a = Parser ([(Integer, t)] -> ParseResult t a)

parse :: Parser t a  -> [t] -> ParseResult t a
parse (Parser p) = p . Prelude.zip [0..]

runParser :: Parser t a -> [(Integer, t)] -> ParseResult t a
runParser (Parser p) = p

combineErrors :: (Ord t) => ParseError t -> ParseError t -> ParseError t
combineErrors (s1, (i1, e1)) (s2, (i2, e2))
    | i1 == i2 = (Set.union s1 s2, (i1, e1)) -- i1 == i2 => e1 == e2
    | i1 > i2 = (s1, (i1, e1))
    | i2 > i1 = (s2, (i2, e2))

combineUncertainty e@(_, (i, _)) m
    | any farEnough m = Success m
    | otherwise = Uncertain e m
      where
        farEnough (_, []) = True
        farEnough (_, ((i2, _):ts)) = i2 > i

combineParses :: (Ord t) =>
    ParseResult t a -> ParseResult t a -> ParseResult t a
combineParses (Error e1) (Error e2) =
    Error (combineErrors e1 e2)
combineParses (Error e1) (Uncertain e2 m) =
    Uncertain (combineErrors e1 e2) m
combineParses (Error e) (Success m) =
    combineUncertainty e m
combineParses (Uncertain e1 m1) (Uncertain e2 m2) =
    Uncertain (combineErrors e1 e2) (m1 <> m2)
combineParses (Uncertain e m1) (Success m2) = 
    combineUncertainty e (m2 <> m1)    
combineParses (Success m1) (Success m2) =
    Success (m1 <> m2)
combineParses a b = combineParses b a

instance (Ord t) => Functor (Parser t) where
    fmap = liftM

instance (Ord t) => Applicative (Parser t) where
    pure a = Parser (\ts -> Success ((a, ts) :| []))
    (<*>) = ap

instance (Ord t) => Monad (Parser t) where
    (>>=) p f = Parser (\ts -> 
        case runParser p ts of
            (Error e) -> Error e
            (Success res1) -> foldl1 combineParses $
                fmap (\(a, ts2) -> runParser (f a) ts2) res1
            (Uncertain e res1) -> foldl combineParses (Error e) $
                fmap (\(a, ts2) -> runParser (f a) ts2) res1
            )

instance (Ord t) => Alternative (Parser t) where
    empty = Parser (\ts -> Error undefined)
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

--With the new error system this seems a bit strange in the uncertain case...
eager :: Parser t a -> Parser t a
eager p = Parser e where
    e ts = case runParser p ts of
        Error e -> Error e
        Success (x :| t) -> Success (x :| [])
        Uncertain e (x :| t) -> Uncertain e (x :| [])

match :: t -> (t -> Bool) -> Parser t t
match expected pred = Parser match' where
    match' [] = error "Input Stream ended unexpectedly!"
    match' ((i,t):ts)
        | pred t = Success ((t,ts) :| [])
        | otherwise = Error (Set.singleton expected, (i,t))

--eof :: (t -> e) -> Parser t e ()
--eof notEmptyError = Parser eof' where
--    eof' [] = Right (((), []) :| [])
--    eof' (t:ts) = Left (notEmptyError t)
