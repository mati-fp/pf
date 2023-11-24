-- Usando list comprehension, defina uma função 
-- matriz :: Int -> Int -> [(Int,Int)] que retorna esta matriz para um 
-- tamanho dado. Por exemplo:
-- > matriz 1 2
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]

matriz :: Int -> Int -> [(Int,Int)]
matriz x y = [(a,b) | a <- [0..x], b <- [0..y] ]

