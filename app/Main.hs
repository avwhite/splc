module Main where

import Lib
import Parser.Parser
import Parser.Combinators
import Parser.Output
import System.Environment
import Parser.Scanner
import Parser.AST
import Parser

import Semantics.Types

import Codegen.Codegen
import Codegen.CodeWrite

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
    let a = infer $ typeInferAst ast (TVar (NamedTV "a"))
    putStrLn (show a)

parseStr p str = 
    case scan str of 
        Success ((ts,cs) :| []) -> case parse (p <* eof) ts of
            (Error errors) -> putStrLn (show errors)
            (Success ((a,ts) :| [])) -> drawAst a
            (Success (_ :| _)) -> putStrLn "Ambigous Parse. Fix you broken gramar!"
        Error errors -> putStrLn (show errors)

parseFile p f = readFile f >>= parseStr p

compiler = do
    [file] <- getArgs
    p <- readFile file
    case parseSpl p of
        (Right ast) -> do
            case infer $ typeInferAst ast (TVar (NamedTV "a")) of
                (Right _) -> do
                    let code = genCode (codeGen ast)
                    writeInstr code
                (Left e) -> do
                    putStrLn (show e)
        (Left e) -> putStrLn e

main :: IO ()
main = compiler
