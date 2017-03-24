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
import Data.Maybe

import Parser
import Parser.AST
import Parser.Output

import Semantics.Types

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

test_mgu = [
        testCase "Pair successfull unification" test1,
        testCase "Pair unsuccessfull unification" test2,
        testCase "Arrow successfull unification" test3,
        testCase "Arrow unsuccessfull unification" test4
    ] where
    test1 = subst u t1 @?= subst u t2  where
        t1 = (TPair (TVar (NamedTV "a")) TBool)
        t2 = (TPair TInt (TVar (NamedTV "b")))
        u = fromJust $  mgu t1 t2
    test2 = mgu t1 t2 @?= Nothing where
        t1 = (TPair (TVar (NamedTV "a")) TBool)
        t2 = (TPair TInt (TVar (NamedTV "a")))
    test3 = subst u t1 @?= subst u t2  where
        t1 = (TArrow [(TVar (NamedTV "a")), TBool] TVoid)
        t2 = (TArrow [TInt, (TVar (NamedTV "b"))] TVoid)
        u = fromJust $ mgu t1 t2
    test4 = mgu t1 t2 @?= Nothing  where
        t1 = (TArrow [(TVar (NamedTV "a")), TBool] TVoid)
        t2 = (TArrow [TInt, (TVar (NamedTV "a"))] TVoid)


test_type_inference =
    [testCase "Int list cannot unify with bool" test1
    ,testCase "Int list infers correctly" test2] where
    test1 = (infer (typeInferExp e mempty a)) @?= Nothing where
        e = Op2E Cons (IntE 1) NilE
        a = TBool
    test2 = subst s a @?= TList TInt where
        s = fromJust (infer (typeInferExp e mempty a))
        e = Op2E Cons (IntE 1) NilE
        a = (TVar (NamedTV "a"))
