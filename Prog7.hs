{- ##################################
   Ryan Skipp
   Homework 7.
################################## -}

module Prog7 where

unique :: Eq a=> [a] -> [a]
unique [] = []
unique (x:xs)
  | x `elem` (unique xs) = [y | y <- (unique xs), y /= x]
  | otherwise            = x:(unique xs)

data Expr1 = Val1 Int
           | Add1 Expr1 Expr1
           | Sub1 Expr1 Expr1

value1 :: Expr1 -> Int
value1 (Val1 a)   = a
value1 (Add1 a b) = (value1 a) + (value1 b)
value1 (Sub1 a b) = (value1 a) - (value1 b)

data Expr2 = Val2 Int
           | Add2 Expr2 Expr2
           | Sub2 Expr2 Expr2
           | Mul2 Expr2 Expr2
           | Div2 Expr2 Expr2 

instance Show Expr2 where
  show (Val2 a) = show a
  show (Add2 a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (Sub2 a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
  show (Mul2 a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
  show (Div2 a b) = "(" ++ show a ++ "/" ++ show b ++ ")"

value2 :: Expr2 -> Maybe Int
value2 (Val2 n)   = Just n
value2 (Add2 a b)
  | (value2 a) /= Nothing && (value2 b) /= Nothing
                  = Just ((getVal(value2 a)) + (getVal(value2 b)))
  | otherwise     = Nothing
value2 (Sub2 a b)
  |(value2 a) /= Nothing && (value2 b) /= Nothing 
                  = Just ((getVal(value2 a)) - (getVal(value2 b)))
  | otherwise     = Nothing
value2 (Mul2 a b) 
  | (value2 a) /= Nothing && (value2 b) /= Nothing
                  = Just ((getVal(value2 a)) * (getVal(value2 b)))
  | otherwise     = Nothing
value2 (Div2 a b)
  | (value2 a) /= Nothing && (value2 b) /= Nothing && (getVal(value2 b)) /= 0
                  = Just ((getVal(value2 a)) `div` (getVal(value2 b)))
  | otherwise     = Nothing

getVal :: Maybe Int -> Int
getVal (Just n)  = n
getVal (Nothing) = 0

piglatinize :: String -> String 
piglatinize [] = ""
piglatinize str = if (hasVowel(head str)) 
        then str ++ "yay"
        else if (not(hasVowel(head(tail(str))))) 
          then ((tail(tail str))) ++ [(head str)] ++ [(head(tail str))] ++ "ay"
        else (tail str) ++ [(head str)] ++ "ay"

hasVowel :: Char -> Bool
hasVowel x = x `elem` ['a','e','i','o','u','A','E','I','O','U']

data Tree a = Leaf a | Node (Tree a) (Tree a)

balanced :: Tree a -> Bool 
balanced (Leaf n) = True
balanced (Node left right)
   |(numLeaves left) - (numLeaves right) == 1    = True 
   |(numLeaves left) - (numLeaves right) == (-1) = True
   |(numLeaves left) - (numLeaves right) == 0    = True
   | otherwise                                   = False

numLeaves :: Tree a -> Int
numLeaves (Leaf n)          = 1
numLeaves (Node left right) = numLeaves left + numLeaves right

data Expr3 = Val3 Int
           | Add3 Expr3 Expr3
           | Sub3 Expr3 Expr3
           | Mul3 Expr3 Expr3
           | Div3 Expr3 Expr3
           | If BExpr3 Expr3 Expr3
  deriving Show

data BExpr3 = BoolLit Bool
            | Or BExpr3 BExpr3
            | EqualTo Expr3 Expr3
            | LessThan Expr3 Expr3
  deriving Show

bEval :: BExpr3 -> Bool
bEval (BoolLit True)  = True
bEval (BoolLit False) = False
bEval (Or a b)
   |bEval a || bEval b     = True
   |otherwise              = False
bEval (EqualTo a b)
   |getVal(value3 a) == getVal(value3 b)    = True
   |otherwise                               = False
bEval (LessThan a b)
   |getVal(value3 a) < getVal(value3 b)     = True
   |otherwise                               = False
   
value3 :: Expr3 -> Maybe Int
value3 (Val3 n)   = Just n
value3 (Add3 a b)
  | (value3 a) /= Nothing && (value3 b) /= Nothing
                  = Just ((getVal(value3 a)) + (getVal(value3 b)))
  | otherwise     = Nothing
value3 (Sub3 a b)
  |(value3 a) /= Nothing && (value3 b) /= Nothing 
                  = Just ((getVal(value3 a)) - (getVal(value3 b)))
  | otherwise     = Nothing
value3 (Mul3 a b) 
  | (value3 a) /= Nothing && (value3 b) /= Nothing
                  = Just ((getVal(value3 a)) * (getVal(value3 b)))
  | otherwise     = Nothing
value3 (Div3 a b)
  | (value3 a) /= Nothing && (value3 b) /= Nothing && (getVal(value3 b)) /= 0
                  = Just ((getVal(value3 a)) `div` (getVal(value3 b)))
  | otherwise     = Nothing
value3 (If x a b)
   |bEval x        = Just(getVal(value3 a))
   |otherwise      = Just(getVal(value3 b))

