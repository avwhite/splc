module Main where

import Lib
import Scanner
import Parser.Parser
import Parser.Output
import System.Environment

testparse :: FilePath -> IO ()
testparse pth = do
    f <- readFile pth
    drawAst $ parse splp f

main :: IO ()
main = do
    input <- getLine
    drawAst (parse funTypep input)
