module Main where

import Lib
import Parser.Parser
import Parser.Combinators
import Parser.Output
import System.Environment
import Parser.Scanner
import Parser.AST

import Data.List.NonEmpty

scanParse p str = 
    case scan str of 
        Success ((ts,cs) :| []) -> case parse (p <* eof) ts of
            (Error errors) -> error (show errors)
            (Success ((a,ts) :| [])) -> a
            (Success (_ :| _)) -> error "Ambigous parse!"
        (Error errors) -> error (show errors)
        (Success (_ :| _)) -> error "Ambigous scan!"

testProgram f = do
    s <- readFile f
    let ast = scanParse splp s
    let prgm = prettyPrint ast
    let ast2 = scanParse splp prgm
    putStrLn (show (ast == ast2))

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
