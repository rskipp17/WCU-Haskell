{- ##################################
   RYAN SKIPP
   Homework 3.
   ################################## -}

module Prog3 where

productLastPart :: Int -> [Int] -> Int
productLastPart n xs
  | length xs == n = product xs
  | length xs > n  = productLastPart n (tail xs)

init' :: [Int] -> [Int]
init' xs = reverse(tail(reverse xs))

init'' :: [Int] -> [Int]
init'' [x]       = []
init'' (x:xs)    = x : (init'' xs) 

elemAt :: Int -> [Int] -> Int
elemAt n (x:xs) 
  | n == 1 = x 
  | n > 1  = elemAt (n-1) xs

numTimes :: Int -> [Int] -> Int
numTimes y [] = 0
numTimes y (x:xs)
  | y == x = 1 + numTimes y xs
  | y /= x = numTimes y xs

lowerFirstLetter :: String -> String
lowerFirstLetter [] = []
lowerFirstLetter (x:xs)
  | x >= 'a' && x <= 'z'   = x : xs
  | x >= 'A' && x <= 'Z'  = (toEnum((fromEnum x) + 32)) : xs
  | otherwise            = x : lowerFirstLetter xs
  
nestedParens :: String -> Bool
nestedParens [] = True
nestedParens x
  | head x == '(' && last x == ')'   = nestedParens(tail(init(x)))
  | otherwise                        = False

triads :: Int -> [(Int,Int,Int)]
triads i = [(x,y,isqrt((x*x) + (y*y))) | x <- [1..i], y <- [1..i], z <- [1..i], x^2 + y^2 == z^2] 

iSort' :: [(Float, Int, String)] -> [(Float, Int, String)]
iSort' [] = []
iSort' ((f,x,s):xs) = ins' (f,x,s) (iSort' xs)

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge x y = mSort (x++y)

-- **Helper Functions** 

-- insert function helper for iSort'
ins' :: (Float, Int, String) -> [(Float, Int, String)] -> [(Float, Int, String)]
ins' (f,x,s) [] = [(f,x,s)]
ins' (f,x,s) ((g,y,t):ys)
  | x < y     = (f,x,s) : (g,y,t) : ys
  | otherwise = (g,y,t) : (ins' (f,x,s) ys)

-- Sorting Algorithm for merge
mSort :: [Int] -> [Int]
mSort [] = []
mSort (x : xs) = mIns x (mSort xs)

-- Insertion algorithm for mSort
mIns :: Int -> [Int] -> [Int]
mIns x [] = [x]
mIns x (y:ys)
  | x > y     = x : y : ys
  | otherwise = y : (mIns x ys)

isqrt :: Int -> Int
isqrt x = floor(sqrt(fromIntegral(x)))
