{- Test Cases for HW 7 -> Prog7.hs
West Chester University
CSC 345 - Programming Language Concepts / Paradigms - Fall 2019
Original format provided by: Richard Burns, distributed with permission.
Authors: Mahmoud Gudarzi, Anton Adamovich, Brandon Barker, Akash Kumar,
    and Cole Gottdank
AUTHORS GIVE NO GUARANTEES THAT TEST CASES ARE CORRECT OR COMPLETE.
INSTRUCTOR HAS FINAL WORD CONCERNING THE FUNCTIONALITY OF YOUR CODE.
YOU ARE ENCOURAGED TO TEST YOUR CODE INDEPENDENTLY.
Usage:
    ghci
    :load Prog7Test
    main
Dependencies:
    cabal update
    cabal install tasty
    cabal install tasty-hunit
-}

import Data.List
import Prog7
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit

class InternalShow a where
    internalShow :: a -> String

assertEqualShow :: (Eq o, Show t, Show o) => t -> o -> (t -> o)  -> Assertion
assertEqualShow input output fn =
    assertEqual
        ("input: " ++ (show input) ++ "\n")
        output
        (fn input)

assertEqualInternal :: (Eq o, InternalShow t, Show o) => t -> o -> (t -> o)  -> Assertion
assertEqualInternal input output fn =
    assertEqual
        ("input: " ++ (internalShow input) ++ "\n")
        output
        (fn input)

data Tree3 = Tree3 String [Tree3]

instance Show Tree3 where
    show = unlines . drawTree3 where
        drawTree3 :: Tree3 -> [String]
        drawTree3 (Tree3 x ts0) = x : drawSubTrees ts0
        drawSubTrees [] = []
        drawSubTrees [t] = "\9474" : shift "\9492 " "   " (drawTree3 t)
        drawSubTrees (t:ts) = "\9474" : shift "\9500 " "\9474  " (drawTree3 t) ++ drawSubTrees ts
        shift first other = zipWith (++) (first : repeat other)

instance Show a => Show (Tree a) where
    show = show . tree1as3 where
        tree1as3 :: Show a => Tree a -> Tree3
        tree1as3 (Leaf x)     = Tree3 (show x) []
        tree1as3 (Node l r) = Tree3 "\9472\9488" [tree1as3 l, tree1as3 r]

instance InternalShow Expr1 where
  internalShow (Val1 n) = show n
  internalShow (Add1 l r) = (internalShow l) ++ " + " ++ (internalShow r)
  internalShow (Sub1 l r) = (internalShow l) ++ " - " ++ (internalShow r)

instance InternalShow Expr2 where
  internalShow (Val2 n) = show n
  internalShow expr = '(':( case expr of
        (Add2 l r) -> (internalShow l) ++ " + " ++ (internalShow r)
        (Sub2 l r) -> (internalShow l) ++ " - " ++ (internalShow r)
        (Mul2 l r) -> (internalShow l) ++ " * " ++ (internalShow r)
        (Div2 l r) -> (internalShow l) ++ " / " ++ (internalShow r)
    ) ++ ")"

main :: IO ()
main = do
    setEnv "TASTY_TIMEOUT" "2s"
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [
        -- 1: unique :: Eq a => [a] -> [a]
        testGroup "test_unique" [
            testCase "test1" $ assertEqualShow "321abcdef123abc123" "def" (sort . unique),
            testCase "test2" $ assertEqualShow "0112234566778899" "0345" (sort . unique),
            testCase "test3" $ assertEqualShow [1, 1, 3, 2, 2, 3, 6, 4, 5, 4, 6, 7, 8, 9, 9] [5, 7, 8] (sort . unique),
            testCase "test4" $ assertEqualShow ['a', 'b', 'c', 'a', 'b', 'd'] ['c', 'd'] (sort . unique),
            testCase "test5" $ assertEqualShow "" "" (sort . unique),
            testCase "test6" $ assertEqualShow [3, 200, 5881, 200, 5881, 3, 1337, 42, 666, 42] [666, 1337] (sort . unique)
        ],

        -- 2: value1 :: Expr1 -> Int
        testGroup "test_value1" [
            testCase "test1" $ assertEqualInternal (Add1 (Add1 (Val1 5) (Val1 10)) (Sub1(Val1 5)(Val1 3))) 17 value1,
            testCase "test2" $ assertEqualInternal (Sub1 (Val1 20) (Val1 22)) (-2) value1,
            testCase "test3" $ assertEqualInternal (Val1 7) 7 value1,
            testCase "test4" $ assertEqualInternal (Sub1 (Val1 0) (Sub1 (Add1 (Val1 3) (Val1 5)) (Val1 12))) 4 value1,
            testCase "test5" $ assertEqualInternal (Sub1 (Add1 (Val1 (-10)) (Val1 4)) (Add1 (Val1 17) (Val1 1))) (-24) value1
        ],

        -- 3 (#4 in PDF): value2 :: Expr2 -> Maybe Int
        testGroup "test_value2" [
            testCase "test1" $ assertEqualInternal (Add2 (Add2 (Val2 5) (Val2 10)) (Sub2(Val2 5)(Val2 3))) (Just 17) value2,
            testCase "test2" $ assertEqualInternal (Sub2 (Val2 20) (Val2 22)) (Just (-2)) value2,
            testCase "test3" $ assertEqualInternal (Val2 7) (Just 7) value2,
            testCase "test4" $ assertEqualInternal (Sub2 (Val2 0) (Sub2 (Add2 (Val2 3) (Val2 5)) (Val2 12))) (Just 4) value2,
            testCase "test5" $ assertEqualInternal (Sub2 (Add2 (Val2 (-10)) (Val2 4)) (Add2 (Val2 17) (Val2 1))) (Just (-24)) value2,
            testCase "test6" $ assertEqualInternal (Sub2 (Mul2 (Val2 (-10)) (Val2 2)) (Div2 (Val2 17) (Val2 5))) (Just (-23)) value2,
            testCase "test7" $ assertEqualInternal (Div2 (Mul2 (Sub2 (Add2 (Val2 (-10)) (Val2 8)) (Val2 (-17))) (Val2 4)) (Val2 20)) (Just 3) value2 ,
            testCase "test8" $ assertEqualInternal (Sub2 (Add2 (Val2 (-10)) (Val2 2)) (Div2 (Val2 17) (Val2 0))) Nothing value2,
            testCase "test9" $ assertEqualInternal (Div2 (Mul2 (Val2 7) (Val2 3)) (Sub2 (Val2 4) (Add2 (Val2 1) (Val2 3)))) Nothing value2,
            testCase "test10" $ assertEqualInternal (Div2 (Val2 11) (Val2 0)) Nothing value2,
            testCase "test11" $ assertEqualInternal (Add2 (Add2 (Val2 (-10)) (Val2 2)) (Div2 (Val2 17) (Val2 0))) Nothing value2,
            testCase "test12" $ assertEqualInternal (Mul2 (Add2 (Val2 (-10)) (Val2 2)) (Div2 (Val2 17) (Val2 0))) Nothing value2
        ],

        -- 4 (#5 in PDF): show :: Expr2 -> String
        testGroup "test_show" [
            testCase "test1" $ assertEqualInternal (Add2 (Add2 (Val2 5) (Val2 10)) (Sub2 (Val2 5)(Val2 3))) "((5+10)+(5-3))" show,
            testCase "test2" $ assertEqualInternal (Sub2 (Val2 20) (Val2 22)) "(20-22)" show,
            testCase "test3" $ assertEqualInternal (Val2 7) "7" show,
            testCase "test4" $ assertEqualInternal (Sub2 (Val2 0) (Sub2 (Add2 (Val2 3) (Val2 5)) (Val2 12))) "(0-((3+5)-12))" show,
            testCase "test5" $ assertEqualInternal (Sub2 (Add2 (Val2 (-10)) (Val2 4)) (Add2 (Val2 17) (Val2 1))) "((-10+4)-(17+1))" show,
            testCase "test6" $ assertEqualInternal (Mul2 (Div2 (Val2 1000) (Val2 4)) (Sub2 (Val2 17) (Val2 50))) "((1000/4)*(17-50))" show,
            testCase "test7" $ assertEqualInternal (Add2 (Mul2 (Val2 10) (Val2 4)) (Div2 (Val2 (-17)) (Val2 0))) "((10*4)+(-17/0))" show
        ],

        --5 (#6 in PDF): piglatinize :: String -> String
        testGroup "test_piglatinize" [
            testCase "test1" $ assertEqualShow "knope" "opeknay" piglatinize,
            testCase "test2" $ assertEqualShow "fiddledeedee" "iddledeedeefay" piglatinize,
            testCase "test3" $ assertEqualShow "haskell" "askellhay" piglatinize,
            testCase "test4" $ assertEqualShow "abstemious" "abstemiousyay" piglatinize,
            testCase "test5" $ assertEqualShow "chocolate" "ocolatechay" piglatinize,
            testCase "test6" $ assertEqualShow "birthday" "irthdaybay" piglatinize,
            testCase "test7" $ assertEqualShow "o" "oyay" piglatinize
        ],

        --6 (#7 in PDF): balanced :: Tree a -> Bool
        testGroup "test_balanced" [
            testCase "test1" $ assertEqualShow (Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')) True balanced,
            testCase "test2" $ assertEqualShow (Node (Node (Node (Leaf 5) (Leaf 5)) (Leaf (-1))) (Leaf 6)) False balanced,
            testCase "test3" $ assertEqualShow (Node (Node (Node (Leaf 5) (Leaf 5)) (Leaf (-1))) (Node (Node (Leaf 5) (Leaf 5)) (Leaf (-1)))) True balanced,
            testCase "test4" $ assertEqualShow (Node (Node (Leaf 5) (Leaf 5)) (Node (Leaf (-1)) (Node (Leaf 10) (Node (Leaf 10) (Leaf 9))))) False balanced,
            testCase "test5" $ assertEqualShow (Node (Node (Node (Leaf 5) (Leaf 5)) (Leaf (-1))) (Node (Node (Leaf 5) (Leaf 5)) (Node (Leaf (-1)) (Leaf 3)))) True balanced,
            testCase "test6" $ assertEqualShow (Node (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 3))) (Leaf (-1))) (Node (Node (Leaf 5) (Leaf 5)) (Node (Leaf (-1)) (Leaf 3)))) False balanced,
            testCase "test7" $ assertEqualShow (Leaf 1) True balanced,
            testCase "test8" $ assertEqualShow (Node (Leaf 'a') (Leaf 'b')) True balanced
        ],

        -- 7 (#9 in PDF): bEval :: BExpr3 -> Bool
        testGroup "test_bEval" [
            testCase "test1" $ assertEqual []  True (bEval (Or (EqualTo (Val3 5) (Div3(Val3 15)(Val3 3))) (BoolLit True))),
            testCase "test2" $ assertEqual []  True (bEval (EqualTo (Val3 (-3)) (Sub3 (Val3 4) (Val3 7)))),
            testCase "test3" $ assertEqual []  True (bEval (LessThan (Val3 (-3)) (Sub3 (Val3 4) (Val3 6)))),
            testCase "test4" $ assertEqual []  False (bEval (BoolLit False)),
            testCase "test5" $ assertEqual []  True (bEval (BoolLit True)),
            testCase "test6" $ assertEqual []  False (bEval (Or (LessThan (Div3(If(EqualTo(Val3 2)(Add3(Val3 2)(Val3 1)))(If(Or(BoolLit True)(BoolLit False))(Val3 25)(Val3 2))(Val3 325))(Val3 25))(Val3 12)) (BoolLit False)))
        ],

        -- 8 (#10 in PDF): value3 :: Expr3 -> Maybe Int
        testGroup "test_value3" [
            testCase "test1" $ assertEqual []  (Just 34) (value3 (Mul3 (Add3 (Add3 (Val3 5) (Val3 10)) (Sub3(Val3 5)(Val3 3))) (Div3 (Val3 10)(Mul3(Val3 1)(Val3 5))))),
            testCase "test3" $ assertEqual []  (Just 3) (value3 (If (BoolLit True)(Add3 (Val3 1) (Val3 2))(Val3 4))),
            testCase "test4" $ assertEqual []  (Just 4) (value3 (If (BoolLit False)(Add3 (Val3 1) (Val3 2))(Val3 4))),
            testCase "test2" $ assertEqual []  Nothing (value3 (Mul3 (Add3 (Add3 (Val3 5) (Val3 10)) (Sub3(Val3 5)(Val3 3))) (Div3 (Val3 10)(Mul3(If (BoolLit False) (Val3 2) (Val3 0))(Val3 5))))),
            testCase "test5" $ assertEqual []  (Just 15) (value3 (Mul3 (If (LessThan (Add3 (Val3 2) (Val3 2)) (Val3 5)) (Div3 (Val3 6)(Val3 2)) (Val3 7)) (Val3 5))),
            testCase "test6" $ assertEqual []  (Just 35) (value3 (Mul3 (If (LessThan (Add3 (Val3 2) (Val3 6)) (Val3 5)) (Div3 (Val3 6)(Val3 2)) (Val3 7)) (Val3 5))),
            testCase "test7" $ assertEqual []  Nothing (value3 (Div3 (Val3 8)(Val3 0))),
            testCase "test8" $ assertEqual []  (Just 1) (value3 (Div3 (If (EqualTo (Val3 3) (Add3 (Val3 2)(Val3 1))) (If (Or (BoolLit True)(BoolLit False)) (Val3 25) (Val3 2)) (Val3 3)) (Val3 25))),
            testCase "test9" $ assertEqual []  (Just 13) (value3 (Div3 (If (EqualTo (Val3 2) (Add3 (Val3 2)(Val3 1))) (If (Or (BoolLit True)(BoolLit False)) (Val3 25) (Val3 2)) (Val3 325)) (Val3 25)))
        ]
    ]