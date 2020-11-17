{- ##################################
   Ryan Skipp
   Homework 4.
   ################################## -}
module Prog4 where

morerecent :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
morerecent (m1,d1,y1) (m2,d2,y2)
  | y1 > y2 = (m1,d1,y1)
  | y2 > y1 = (m2,d2,y2)
  | m1 > m2 = (m1,d1,y1)
  | m2 > m1 = (m2,d2,y2)
  | d1 > d2 = (m1,d1,y1)
  | d2 > d1 = (m2,d2,y2)
  | otherwise = (m1,d1,y1) -- same date

numInMonth :: [(Int, Int, Int)] -> Int -> Int 
numInMonth ds m = sum[1 | (m1,d1,y1) <- ds, m1 == m]

datesInMonth :: [(Int, Int, Int)] -> Int -> [(Int,Int,Int)]
datesInMonth ds m = [(m1,d1,y1) | (m1,d1,y1) <- ds, m1 == m]

month2Str :: (Int, Int, Int) -> String
month2Str (m,_,_)
  | m == 1  = "January"
  | m == 2  = "February"
  | m == 3  = "March"
  | m == 4  = "April"
  | m == 5  = "May"
  | m == 6  = "June"
  | m == 7  = "July"
  | m == 8  = "August"
  | m == 9  = "September"
  | m == 10 = "October"
  | m == 11 = "November"
  | m == 12 = "December"

date2Str :: (Int, Int, Int) -> String
date2Str (m,d,y) = month2Str (m,d,y) ++ " " ++ show d ++ ", " ++ show y

monthLookup :: Int -> Int
monthLookup x
  | x <= 31  = 1
  | x <= 59  = 2
  | x <= 90  = 3
  | x <= 120 = 4
  | x <= 151 = 5
  | x <= 181 = 6
  | x <= 212 = 7
  | x <= 243 = 8
  | x <= 273 = 9
  | x <= 304 = 10
  | x <= 334 = 11
  | x <= 365 = 12

monthRange :: Int -> Int -> [Int]
monthRange s e = [x..y]
  where 
   x = monthLookup s
   y = monthLookup e

validDate :: (Int, Int, Int) -> Bool
validDate (m,d,y)
  | m > 12 || m < 1   = False
  | d < 1 && d > 31   = False
  | y < 1             = False
  | m == 2 && d > 28  = False
  | m == 4 && d > 30  = False
  | m == 6 && d > 30  = False
  | m == 9 && d > 30  = False
  | m == 11 && d > 30 = False  
  | otherwise         = True

validLeapDate :: (Int, Int, Int) -> Bool
validLeapDate (m,d,y)
  | m /= 2 || d /= 29 = False
  | y `mod` 100 == 0  = False
  | y `mod` 400 == 0 || y `mod` 4 == 0 = True
  | otherwise         = False

season :: (Int, Int, Int) -> String
season (m,d,y)
  | x >= spS && x <= spE = "Spring"
  | x >= smS && x <= smE = "Summer"
  | x >= fS  && x <= fE  = "Fall"
  | otherwise            = "Winter"
  where
    x   = dateToNum(m,d)
    spS = dateToNum(3,20)
    spE = dateToNum(6,20)
    smS = dateToNum(6,21)
    smE = dateToNum(9,22)
    fS  = dateToNum(9,23)
    fE  = dateToNum(12,21)
    wS  = dateToNum(12,22)
    wE  = dateToNum(3,19)

-- Helper Function: Returns day (1-365) given date
dateToNum :: (Int,Int) -> Int  
dateToNum (m,d)  
  | m == 1  = 0 + d 
  | m == 2  = 31 + d
  | m == 3  = 59 + d
  | m == 4  = 90 + d
  | m == 5  = 120 + d
  | m == 6  = 151 + d
  | m == 7  = 181 + d
  | m == 8  = 212 + d
  | m == 9  = 243 + d
  | m == 10 = 273 + d
  | m == 11 = 304 + d
  | m == 12 = 334 + d
