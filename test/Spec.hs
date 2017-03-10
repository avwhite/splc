module Spec where

import System.Directory
import System.FilePath

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Parser
import Parser.Output

pppTestFile f = do
    prgrm1 <- readFile f
    let ast1 = parseSpl prgrm1
    let prgrm2 = fmap prettyPrint ast1
    let ast2 = (prgrm2 >>= \p -> parseSpl p)
    case ast2 of 
        Left e -> assertFailure (show e)
        Right _ -> ast1 @=? ast2

test_ppp_on_syntactically_correct_programs :: IO [TestTree]
test_ppp_on_syntactically_correct_programs = do
    let dir = "testdata/syntax_valid_spl"
    fileNames <- listDirectory dir
    let cases = fmap (dir </>) fileNames
    pure $ fmap (\s -> testCase s (pppTestFile s)) cases
