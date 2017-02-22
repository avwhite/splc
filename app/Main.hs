module Main where

import Lib
import Scanner
import Parser.Parser
import Parser.Output
import System.Environment


main :: IO ()
main = do
    input <- getLine
    drawAst (parse funTypep input)
