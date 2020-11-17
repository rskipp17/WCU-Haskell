{- ##################################
   Ryan Skipp
   Homework 2
   ################################## -}

module Prog2 where

threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent x y z = and[x /= y, y /= z, x /= z]

sum' :: Integer -> Integer
sum' x
  | x == 0   = 0
  | x > 0    = x + sum'(x-1)

abssum :: Integer -> Integer -> Integer
abssum m n
  | m == n   = 0
  | m <= n   = abs m + abssum (m+1) n 

integerSqrt :: Integer -> Integer
integerSqrt x = floor(sqrt(fromIntegral x))

exponent' :: Integer -> Integer -> Integer
exponent' x y
  | y == 0    = 1
  | otherwise = x * exponent' x (y-1)

largeSmall :: (Integer, Integer, Integer) -> (Integer, Integer)
largeSmall (x,y,z)
  | threeMax x y z == x    = (x, twoMax y z)
  | threeMax x y z == y    = (y, twoMax x z)
  | threeMax x y z == z    = (z, twoMax x y)

swap :: (Char, Char, Char, Char) -> (Char, Char, Char, Char)
swap (a,b,c,d) = (a,c,b,d)

negateOdds :: [Integer] -> [Integer]
negateOdds list = (evenList list) ++ (oddList list) 

matches :: Integer -> [Integer] -> [Integer]
matches x list = [ xs | xs <- list, x == xs] 

element :: Integer -> [Integer] -> Bool
element x list = matches x list /= []

-- HELPER FUNCTIONS FROM CLASS
twoMax x y
  | x >= y    = x
  | otherwise = y

threeMax x y z
  | (twoMax x y) >= z  = twoMax x y
  | otherwise          = z

-- create list with just evens (for negateOdds)
evenList :: [Integer] -> [Integer]
evenList list = [ xs | xs <- list, xs `mod ` 2 == 0]

-- create list with negated odds (for negateOdds)
oddList :: [Integer] -> [Integer]
oddList list = [ (-xs) | xs <- list, xs `mod ` 2 /= 0]
