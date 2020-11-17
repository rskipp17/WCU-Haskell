{- ##################################
   Ryan Skipp
   Homework 6.
   ################################## -}

module Prog6 where

data Tree1 = Leaf1 Int
           | Node1 Int Tree1 Tree1

preorder :: Tree1 -> [Int]
preorder (Leaf1 a) = [a]
preorder (Node1 x left right) = [x] ++ preorder left ++ preorder right

postorder :: Tree1 -> [Int]
postorder (Leaf1 a) = [a]
postorder (Node1 x left right) = postorder left ++
                                 postorder right ++ [x]

sumPositives :: Tree1 -> Int
sumPositives tree = sum[x | x <- preorder(tree), x >= 0]

countLeaves :: Tree1 -> Int
countLeaves (Leaf1 _) = 1
countLeaves (Node1 _ left right) = 0 + (countLeaves left) + (countLeaves right)

depth :: Tree1 -> Int
depth (Leaf1 _)                    = 1
depth (Node1 _ left right)
  |(depth left) >= (depth right)   = (depth left) + 1
  |otherwise                       = (depth right) + 1

data Tree2 a = Leaf2 a
             | Node2 [Tree2 a]

occurs :: Eq a => a -> Tree2 a -> Bool
occurs x (Leaf2 y)      = x == y
occurs x (Node2 (y:ys)) = occurs x y || occurs x (Node2 ys)

countInteriorNodes :: Tree2 a -> Int
countInteriorNodes (Leaf2 n)    = 0
countInteriorNodes (Node2 [])   = 0
countInteriorNodes (Node2 tree) = 1 + 
                             sum [countInteriorNodes x | x <- tree, isNode x]

isNode :: Tree2 a -> Bool
isNode (Leaf2 _) = False
isNode (Node2 _) = True

sumTree :: Tree2 Int -> Int
sumTree (Leaf2 x)      = x
sumTree (Node2 [])     = 0
sumTree (Node2 (y:ys)) = sumTree y + sumTree (Node2 ys)

pre2 :: Tree2 a -> [a]
pre2 (Leaf2 x)      = [x]
pre2 (Node2 [])     = []
pre2 (Node2 (y:ys)) = pre2 y ++ pre2 (Node2 ys)

depthK :: Int -> Tree2 a -> [a]
depthK 0 (Leaf2 n) = [n]
depthK w (Leaf2 _) = []
depthK w (Node2 []) = []
depthK w (Node2 (y:ys)) = (depthK (w-1) (y)) ++ (depthK w (Node2 ys))

t1 :: Tree1
t1 = (Node1 5 (Leaf1 3) (Node1 4 (Leaf1 4) (Leaf1 7))) 

t2 :: Tree1
t2 = (Leaf1 5)

t3 :: Tree2 Int
t3 = Node2 [Node2 [(Leaf2 5), (Leaf2 6)], (Leaf2 4)] 

t4 :: Tree2 Int
t4 = Node2 [Node2 [Node2[(Leaf2 1), (Leaf2 2), (Leaf2 3)], Node2[(Leaf2 4)]]]

