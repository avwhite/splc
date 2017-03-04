module Main where

import Lib
import Scanner
import Parser.Parser
import Parser.Combinators
import Parser.Output
import System.Environment

import Data.List.NonEmpty

parse p str = case runParser (p <* seof) (Prelude.zip [0..] (alexScanTokens str)) of
        (Left errors) -> putStrLn (show errors)
        (Right ((a,ts) :| [])) -> drawAst a
        (Right (_ :| _)) -> putStrLn "Ambigous Parse. Fix you broken gramar!"

parseFile p f = readFile f >>= parse p

main :: IO ()
main = putStrLn "Not doing anything yet. Run inside REPL to test stuff"
