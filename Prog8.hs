{- ###############
   Ryan Skipp
   Homework 8
############### -}

module Prog8 where

sumSqNeg :: [Int] -> Int
sumSqNeg xs = foldr (+) 0 (map (^2) (filter (<0 ) xs))

containing :: Eq a => [a] -> [a] -> Bool
containing x [] = False
containing [] y = True
containing (x:xs) y
  | x `elem` y = containing xs y
  | otherwise  = False

total :: (Int -> Int) -> [Int] -> Int
total f x = sum (map f x)

containing' :: Eq a => [a] -> [a] -> Bool
containing' _ [] = False
containing' xs ys = foldr (&&) True (map elem' xs)
  where
    elem' v
      |v `elem` ys        = True
      |otherwise          = False

lengths :: [String] -> [Int]
lengths xs = map length xs

product' :: Num a => [a] -> a
product' xs = foldr (*) 1 xs 

max' :: Ord a => [a] -> a
max' x = foldr max (last x) x

append' :: [a] -> [a] -> [a]
append' x y = foldr (:) y x 

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst f xs = foldr (:) [] (help f xs)
  where
    help _ [] = []
    help a (z:zs)
      | f z       = z:help f zs
      | otherwise = zs

filterLast :: (a-> Bool) -> [a] -> [a]
filterLast f xs = reverse (foldr (:) [] (filterFirst f (reverse xs)))
