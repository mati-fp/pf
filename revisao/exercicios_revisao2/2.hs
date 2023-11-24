-- Considere a seguinte definição de árvore com dados somente nas folhas:
-- data Tree = Leaf Int | Node Tree Tree deriving (Show)
-- Construa as seguintes funções:
-- a) leaves :: Tree -> [Int] que retorna uma lista contendo os valores que estão nas 
-- folhas da árvore.
-- b) size :: Tree -> Int que retorna o número de folhas da árvore.
-- c) balanced :: Tree -> Bool que decide se a árvore é balanceada ou não, ou seja, se ambas 
-- subárvores diferem no número de folhas em no máximo uma unidade e são balanceadas.

data Tree = Leaf Int | Node Tree Tree deriving (Show)

leaves :: Tree -> [Int]
leaves (Leaf a) = [a]
leaves (Node l r) = leaves l ++ leaves r

size :: Tree -> Int
size (Leaf a) = 1
size (Node l r) = size l + size r

balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node l r) = abs(size l - size r) <= 1 && balanced l && balanced r
