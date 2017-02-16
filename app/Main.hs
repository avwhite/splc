module Main where

import Lib
import Scanner
import Parser.Parser
import Parser.Output
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG
import System.Environment


main :: IO ()
main = do
    input <- getLine
    mainWith $ (graphicalData (parseSpl input) :: Diagram B)
