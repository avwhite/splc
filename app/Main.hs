module Main where

import Lib
import Scanner
import Parser.Parser
import Parser.Combinators
import Parser.Output
import System.Environment

import Data.List.NonEmpty

parseStr p str = case parse (p <* seof) (alexScanTokens str ++ [EofTok]) of
        (Error errors) -> putStrLn (show errors)
        (Success ((a,ts) :| [])) -> drawAst a
        (Success (_ :| _)) -> putStrLn "Ambigous Parse. Fix you broken gramar!"

parseFile p f = readFile f >>= parseStr p

main :: IO ()
main = putStrLn "Not doing anything yet. Run inside REPL to test stuff"
