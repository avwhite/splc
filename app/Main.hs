module Main where

import Lib
import Parser.Parser
import Parser.Combinators
import Parser.Output
import System.Environment
import Parser.Scanner
import Parser.AST

import Data.List.NonEmpty

parseStr p str = 
    case scan str of 
        Success ((ts,cs) :| []) -> case parse (p <* eof) ts of
            (Error errors) -> putStrLn (show errors)
            (Success ((a,ts) :| [])) -> drawAst a
            (Success (_ :| _)) -> putStrLn "Ambigous Parse. Fix you broken gramar!"
        Error errors -> putStrLn (show errors)

parseFile p f = readFile f >>= parseStr p

main :: IO ()
main = putStrLn "Not doing anything yet. Run inside REPL to test stuff"
