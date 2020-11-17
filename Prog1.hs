{- ##################################
   Ryan Skipp
   Homework 1
   ################################## -}

module Prog1 where

isSingleDigit :: Integer -> Bool
isSingleDigit x = x > (-10) && x < 10

dividesEvenly :: Integer -> Integer -> Bool
dividesEvenly x y = (mod x y) == 0

middle :: Integer -> Integer -> Integer -> Integer
middle x y z
  | threeMax x y z == x = twoMax y z
  | threeMax x y z == y = twoMax x z
  | threeMax x y z == z = twoMax x y

nand :: Bool -> Bool -> Bool
nand x y = not(x && y)

triangleArea :: Integer -> Integer -> Float
triangleArea b h = 0.5 * fromIntegral(b) * fromIntegral(h)

floorDecimal :: Float -> Float 
floorDecimal x = fromInteger(floor x)

isNotALetter :: Char -> Bool
isNotALetter x = not(x>='A' && x<='Z') && not(x>='a' && x<='z')

letterGrade :: Integer -> String
letterGrade x
  | x >= 93 = "A"
  | x >= 90 = "A-"
  | x >= 87 = "B+"
  | x >= 83 = "B"
  | x >= 80 = "B-"
  | x >= 77 = "C+"
  | x >= 73 = "C"
  | x >= 70 = "C-"
  | x >= 67 = "D+"
  | x >= 63 = "D"
  | x >= 60 = "D-"
  | x < 60  = "F"

averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z = (fromIntegral(x + y + z) / 3)

howManyBelowAverage :: Integer -> Integer -> Integer -> Integer
howManyBelowAverage x y z
  | floor(averageThree x y z) > x && floor(averageThree x y z) > y && floor(averageThree x y z) > z    = 3
  | floor(averageThree x y z) > x && floor(averageThree x y z) > y                                     = 2
  | floor(averageThree x y z) > y && floor(averageThree x y z) > z                                     = 2
  | floor(averageThree x y z) > x && floor(averageThree x y z) > z                                     = 2
  | floor(averageThree x y z) > x                                                                      = 1
  | floor(averageThree x y z) > y                                                                      = 1
  | floor(averageThree x y z) > z                                                                      = 1
  | otherwise                                                                                          = 0

-- HELPER FUNCTIONS FROM CLASS
twoMax x y
  | x >= y    = x
  | otherwise = y

threeMax x y z
  | (twoMax x y) >= z  = twoMax x y
  | otherwise          = z

