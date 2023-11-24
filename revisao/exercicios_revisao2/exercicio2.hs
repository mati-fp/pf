data Tree = Leaf Int | Node Tree Tree deriving (Show)

leaves :: Tree -> [Int]
leaves (Leaf n) = [n]
leaves (Node e d) = leaves e ++ leaves d

size :: Tree -> Int
size (Leaf _) = 1
size (Node e d) = size e + size d

balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node e d) = abs (size e - size d) <= 1 && balanced e && balanced d

