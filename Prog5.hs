{- ##############
   Ryan Skipp
   Homework 5.	
#################-}

module Prog5 where

reverse' :: [a] -> [a]
reverse' x = case x of
  [] -> []
  (x:xs) -> (reverse' xs) ++ [x]

isPalindrome :: String -> Bool
isPalindrome x = case x of
  [] -> False
  x  -> (reverse' x) == x
  
safeFindAfter :: String -> [String] -> Maybe [String]
safeFindAfter _ [] = Nothing
safeFindAfter s (y:ys)
  | y == s       = Just ys
  | otherwise    = safeFindAfter s ys

data Set = Set [Char]
         | EmptySet
     deriving Show

member :: Char -> Set -> Bool
member _ (EmptySet) = False
member _ (Set []) = False
member a (Set(x:xs))
  | a == x    = True 
  | a /= x    = member a (Set(xs))
  | otherwise = False

size :: Set -> Int
size (EmptySet)  = 0
size (Set [])    = 0
size (Set(x:xs)) = 1 + size(Set(xs))

add :: Char -> Set -> Set
add x (EmptySet) = (Set(x:[]))
add x (Set(y))
  | (member x (Set(y))) == True = (Set(y))
  | otherwise                   = (Set(x:y))

equal :: Set -> Set -> Bool
equal (EmptySet) (EmptySet) = True
equal (EmptySet) _          = False
equal  _ (EmptySet)         = False
equal (Set []) _            = True
equal (Set(x:xs)) (Set(ys))
  | member x (Set(ys)) = equal (Set(xs)) (Set(ys))
  | otherwise          = False

saferemove :: Char -> Set -> Maybe Set
saferemove _ (EmptySet) = Nothing
saferemove a (Set(x:xs)) = Just(Set(ys)) where
                           ys = [y | y <- xs, y /= a]

union :: Set -> Set -> Set
union (EmptySet) (EmptySet) = (EmptySet)
union (EmptySet) (Set(ys))         = (Set(ys))
union (Set(xs)) (EmptySet)         = (Set(xs))
union (Set[]) (Set(ys))            = (Set(ys))
union (Set(x:xs)) (Set(ys))
  | x `elem` ys = union (Set(xs)) (Set(ys))
  | otherwise   = union (Set(xs)) (Set(x:ys))

intersection :: Set -> Set -> Set
intersection (EmptySet) (EmptySet)        = (EmptySet)
intersection (EmptySet) (Set(ys))         = (EmptySet)
intersection (Set(xs)) (EmptySet)         = (EmptySet)
intersection (Set(xs)) (Set(ys)) = Set(zs) where
                                   zs = [z | z <- xs, z `elem` ys]
