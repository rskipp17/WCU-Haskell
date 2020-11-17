{- Test Cases for HW 8 -> Prog8.hs
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
    :load Prog8Test
    main
Dependencies:
    cabal update
    cabal install tasty
    cabal install tasty-hunit
-}

import           Prog8
import           System.Environment
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = do
    setEnv "TASTY_TIMEOUT" "2s"
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

assertFloat :: (Eq a, Show a, Ord a, Fractional a) =>  a -> a -> Assertion
assertFloat t v = assertBool msg (abs (t - v) < 1.0e-5)
    where msg = "Expecting " ++ show v ++ " got " ++ show t ++ " a difference of " ++ show (t - v)

unitTests :: TestTree
unitTests = testGroup "Unit tests" [
        -- 1: sumSqNeg :: [Int] -> Int
        testGroup "test_sumSqNeg" [
            testCase "test1" $ assertEqual [] 0 (sumSqNeg []),
            testCase "test2" $ assertEqual [] 0 (sumSqNeg [1]),
            testCase "test3" $ assertEqual [] 0 (sumSqNeg [1,2,3]),
            testCase "test4" $ assertEqual [] 14 (sumSqNeg [1,-2,-3,4,-1,9]),
            testCase "test5" $ assertEqual [] 133 (sumSqNeg [-2,-11,2,-2,2,-2,1000,100,10]),
            testCase "test6" $ assertEqual [] 875329 (sumSqNeg [11,-932,0,1,-12,14,5,-81])
        ],

        -- 2. containing :: Eq a => [a] -> [a] -> Bool
        testGroup "test_containing" [
            testCase "test1" $ assertEqual [] True (containing ([]::[Int]) ([]::[Int])),
            testCase "test2" $ assertEqual [] True (containing [1] [1]),
            testCase "test3" $ assertEqual [] True (containing [] [1]),
            testCase "test4" $ assertEqual [] True (containing [1,2,3,4,42] [1,2,3,4,42]),
            testCase "test5" $ assertEqual [] True (containing [True,False] [True,False,True]),
            testCase "test6" $ assertEqual [] True (containing ['a','#','3','h'] ['a','#','3','h','y']),
            testCase "test7" $ assertEqual [] True (containing ["aa","#4","323","hello jELlO","yUm yUM"] ["aa","#4","323","hello jELlO","yUm yUM"]),
            testCase "test8" $ assertEqual [] False (containing ["imaginary"] ["real","rational"]),
            testCase "test9" $ assertEqual [] False (containing [1,2,3,4,5] [1,2,3,4]),
            testCase "test10" $ assertEqual [] False (containing [1,2,3,4,5] [2,3,4,5,95]),
            testCase "test11" $ assertEqual [] False (containing [1,2,3,4,5] [2,3,4,5]),
            testCase "test12" $ assertEqual [] True (containing [1,1,1] [1,1]),
            testCase "test13" $ assertEqual [] False (containing [1,2,3,4,5] [])
        ],

        -- 3. total :: (Int -> Int) -> [Int] -> Int
        testGroup "test_total" [
            testCase "test1" $ assertEqual [] 0 (total (* 2) []),
            testCase "test2" $ assertEqual [] 12 (total (* 2) [1,2,3]),
            testCase "test3" $ assertEqual [] 92 (total (+ 3) [11,15,12,42]),
            testCase "test4" $ assertEqual [] 68 (total (+ (-3)) [11,15,12,42]),
            testCase "test5" $ assertEqual [] 65 (total (`div` 2) [4,5,12,13,98]),
            testCase "test6" $ assertEqual [] 0 (total (`div` 200) [4,5,12,13,98]),
            testCase "test7" $ assertEqual [] 126 (total (^ 2) [3,6,9]),
            testCase "test8" $ assertEqual [] 44732898 (total (^ 2 ^ 3) [3,6,9]),
            testCase "test9" $ assertEqual [] (-23) (total (1 -) [2,3,5,7,11])
        ],

        -- 4. lengths :: [String] -> [Int]
        testGroup "test_containing'" [
            testCase "test1" $ assertEqual [] True (containing ([]::[Int]) ([]::[Int])),
            testCase "test2" $ assertEqual [] True (containing' [1] [1]),
            testCase "test3" $ assertEqual [] True (containing' [] [1]),
            testCase "test4" $ assertEqual [] True (containing' [1,2,3,4,42] [1,2,3,4,42]),
            testCase "test5" $ assertEqual [] True (containing' [True,False] [True,False,True]),
            testCase "test6" $ assertEqual [] True (containing' ['a','#','3','h'] ['a','#','3','h','y']),
            testCase "test7" $ assertEqual [] True (containing' ["aa","#4","323","hello jELlO","yUm yUM"] ["aa","#4","323","hello jELlO","yUm yUM"]),
            testCase "test8" $ assertEqual [] False (containing' ["imaginary"] ["real","rational"]),
            testCase "test9" $ assertEqual [] False (containing' [1,2,3,4,5] [1,2,3,4]),
            testCase "test10" $ assertEqual [] False (containing' [1,2,3,4,5] [2,3,4,5,95]),
            testCase "test11" $ assertEqual [] False (containing' [1,2,3,4,5] [2,3,4,5]),
            testCase "test12" $ assertEqual [] True (containing' [1,1,1] [1,1]),
            testCase "test13" $ assertEqual [] False (containing' [1,2,3,4,5] [])
        ],

        -- 5. lengths :: [String] -> [Int]
        testGroup "test_lengths" [
            testCase "test1" $ assertEqual [] [] (lengths []),
            testCase "test2" $ assertEqual [] [0,0,0] (lengths ["","",""]),
            testCase "test3" $ assertEqual [] [4,2,4,3,4,3] (lengths ["This","is","Bob.","Bob","says","hi."]),
            testCase "test4" $ assertEqual [] [3,5,2,5,5] (lengths ["The","world","is","quiet","here."]),
            testCase "test5" $ assertEqual [] [182] (lengths ["Lopadotemachoselachogaleokranioleipsanodrimhypotrimmatosilphioparaomelitokatakechymenokichlepikossyphophattoperisteralektryonoptekephalliokigklopeleiolagoiosiraiobaphetraganopterygon"]),
            testCase "test6" $ assertEqual [] [1,0,2,3] (lengths ["1","","20","345"]),
            testCase "test7" $ assertEqual [] [4,7,10,18] (lengths ["$@#!","&%&#*@(","+__DWJ(H*@", "><POO?@)@()#&!_+-="])
        ],

        -- 6. product' :: Num a => [a] -> a
        testGroup "test_product'" [
            testCase "test1" $ assertEqual [] 0 (product' [0]),
            testCase "test2" $ assertEqual [] 1 (product' [1]),
            testCase "test3" $ assertEqual [] 6 (product' [1,2,3]),
            testCase "test4" $ assertEqual [] (-216) (product' [1,-2,-3,4,-1,9]),
            testCase "test5" $ assertEqual [] 352000000 (product' [2,11,2,2,2,2,1000,100,10]),
            testCase "test6" $ assertEqual [] 0 (product' [11,-932,0,1,-12,14,5,-81]),
            testCase "test7" $ assertEqual [] 697546080 (product' [11,-932,1,-12,14,5,81]),
            testCase "test8" $ assertFloat 23.31 (product' [23.31] ),
            testCase "test9" $ assertFloat 0.0 (product' [18.91, 0.0] ),
            testCase "test10" $ assertFloat (-246.6414) (product' [15.78, (-15.63)] ),
            testCase "test11" $ assertFloat (-53.490624) (product' [(-8.17), 0.48, 13.64] ),
            testCase "test12" $ assertFloat 139157.91828932002 (product' [(-23.41), 16.03, (-20.11), 18.44] ),
            testCase "test13" $ assertFloat 82352.4731568 (product' [(-16.57), (-15.4), 13.4, 1.8, 13.38] ),
            testCase "test14" $ assertFloat (-5121.3108390444) (product' [(-1.76), 3.81, 16.45, (-0.49), (-5.65), 16.77] ),
            testCase "test15" $ assertFloat (-10189.53064429209) (product' [(-5.85), (-2.77), (-3.15), (-16.22), (-6.39), (-2.14), (-0.9)] ),
            testCase "test16" $ assertFloat (-379769736.48264384) (product' [(-12.57), (-14.69), 13.78, 23.01, (-12.35), (-15.24), 12.67, (-2.72)] ),
            testCase "test17" $ assertFloat (-1055809.4528509898) (product' [(-19.17), 1.2, 5.66, (-6.08), (-0.77), 7.94, 21.74, 8.88, 1.13] ),
            testCase "test18" $ assertFloat (-1888.2484863498382) (product' [2.27, (-0.86), 3.07, (-1.37), 3.73, 1.0, (-3.37), (-2.75), (-3.52), 1.89] ),
            testCase "test19" $ assertFloat 4.801524738200703 (product' [2.4, 4.19, (-4.93), (-2.17), 0.35, 0.56, (-0.5), (-3.78), 0.28, (-3.31), (-0.13)] ),
            testCase "test20" $ assertFloat 40.14804395272764 (product' [3.64, 3.37, (-0.9), (-2.0), 0.18, 3.34, 2.11, (-1.7), (-0.75), (-0.13), 2.13, (-4.06)] ),
            testCase "test21" $ assertFloat 20579.073812942548 (product' [4.45, 3.83, (-0.56), 2.13, 2.24, 2.67, 0.52, 3.36, 1.96, 4.89, (-2.86), (-0.93), (-3.8)] ),
            testCase "test22" $ assertFloat (-6.765471094369319) (product' [(-0.47), (-0.1), (-0.12), 4.0, (-4.2), (-0.51), (-1.03), (-4.04), 4.45, 0.52, 2.45, (-4.71), 2.1, (-0.6)] )
        ],

        -- 7. max' :: [String] -> [Int]
        testGroup "test_max'" [
            testCase "test1" $ assertEqual [] 1 (max' [1]),
            testCase "test2" $ assertEqual [] 2.0 (max' [1.0, 2.0]),
            testCase "test3" $ assertEqual [] 5 (max' [3, 5, (-9), 1, 3]),
            testCase "test4" $ assertEqual [] (-100.0) (max' [(-100.0), (-101.1), (-200.0)]),
            testCase "test5" $ assertEqual [] (-100) (max' [(-100), (-101), (-200)]),
            testCase "test6" $ assertEqual [] 'z' (max' ['a', 'b', 'z', 'e']),
            testCase "test7" $ assertEqual [] "z" (max' ["z", "aa", "abc", "def"])
        ],

        -- 8. append' :: [a] -> [a] -> [a]
        testGroup "test_append'" [
            testCase "test1" $ assertEqual [] ([]::[Int]) (append' [] []),
            testCase "test2" $ assertEqual [] [1] (append' [] [1]),
            testCase "test3" $ assertEqual [] [1] (append' [1] []),
            testCase "test4" $ assertEqual [] [(-2), 3, 4, 5, (-10)] (append' [(-2), 3] [4, 5, (-10)]),
            testCase "test5" $ assertEqual [] [2,1,4,3,5] (append' [2,1,4] [3,5]),
            testCase "test6" $ assertEqual [] "pubnub" (append' "pub" "nub"),
            testCase "test7" $ assertEqual [] "az" (append' "a" "z"),
            testCase "test8" $ assertEqual [] "az" (append' "" "az"),
            testCase "test9" $ assertEqual [] [4.0, 1.0, (-2.0), (-3.0)] (append' [] [4.0, 1.0, (-2.0), (-3.0)]),
            testCase "test10" $ assertEqual [] [4.0, 1.0, (-2.0), (-3.0)] (append' [4.0, 1.0] [(-2.0), (-3.0)]),
            testCase "test11" $ assertEqual [] "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed" (append' "Lorem ipsum " "dolor sit amet, consectetur adipiscing elit, sed")
        ],

        -- 9. filterFirst :: (a -> Bool) -> [a] -> [a]
        testGroup "test_filterFirst" [
            testCase "test1" $ assertEqual "(true for all elements)" ([]::[Int]) (filterFirst (const True) []),
            testCase "test2" $ assertEqual "(false for all elements)" ([]::[Int]) (filterFirst (const False) []),
            testCase "test3" $ assertEqual "(true for all elements)" "a" (filterFirst (const True) "a"),
            testCase "test4" $ assertEqual "(false for all elements)" "" (filterFirst (const False) "a"),
            testCase "test5" $ assertEqual "(true for all elements)" "abcd" (filterFirst (const True) "abcd"),
            testCase "test6" $ assertEqual "(false for all elements)" "bcd" (filterFirst (const False) "abcd"),
            testCase "test7" $ assertEqual [] [4] (filterFirst (>= 2) [1,4]),
            testCase "test8" $ assertEqual [] [2,3,4] (filterFirst (>= 2) [1,2,3,4]),
            testCase "test9" $ assertEqual [] [2,3,1] (filterFirst (>= 2) [1,2,3,1]),
            testCase "test10" $ assertEqual [] [3,1] (filterFirst (== 3) [3,2,1]),
            testCase "test11" $ assertEqual [] [3,1] (filterFirst (== 1) [2,3,1]),
            testCase "test12" $ assertEqual [] [1,1,1] (filterFirst (== 1) [1,1,1]),
            testCase "test13" $ assertEqual [] [10,5,8] (filterFirst (> 0) [10,5,8]),
            testCase "test14" $ assertEqual [] [10,9,12,8] (filterFirst even [3,10,9,12,8]),
            testCase "test15" $ assertEqual [] [3,9,12,8] (filterFirst odd [3,10,9,12,8])
        ],

        -- 10. filterLast :: (a -> Bool) -> [a] -> [a]
        testGroup "test_filterLast" [
            testCase "test1" $ assertEqual "(true for all elements)" ([]::[Int]) (filterLast (const True) []),
            testCase "test2" $ assertEqual "(false for all elements)" ([]::[Int]) (filterLast (const False) []),
            testCase "test3" $ assertEqual "(true for all elements)" "a" (filterLast (const True) "a"),
            testCase "test4" $ assertEqual "(false for all elements)" "" (filterLast (const False) "a"),
            testCase "test5" $ assertEqual "(true for all elements)" "abcd" (filterLast (const True) "abcd"),
            testCase "test6" $ assertEqual "(false for all elements)" "abc" (filterLast (const False) "abcd"),
            testCase "test7" $ assertEqual [] [4] (filterLast (>= 2) [1,4]),
            testCase "test8" $ assertEqual [] [2,3,4] (filterLast (>= 2) [1,2,3,4]),
            testCase "test9" $ assertEqual [] [1,2,3] (filterLast (>= 2) [1,2,3,1]),
            testCase "test10" $ assertEqual [] [3,2] (filterLast (== 3) [3,2,1]),
            testCase "test11" $ assertEqual [] [2,1] (filterLast (== 1) [2,3,1]),
            testCase "test12" $ assertEqual [] [1,1,1] (filterLast (== 1) [1,1,1]),
            testCase "test13" $ assertEqual [] [10,5,8] (filterLast (> 0) [10,5,8]),
            testCase "test14" $ assertEqual [] [3,10,12,8] (filterLast even [3,10,9,12,8]),
            testCase "test15" $ assertEqual [] [3,10,9,12] (filterLast odd [3,10,9,12,8])
        ]
    ]