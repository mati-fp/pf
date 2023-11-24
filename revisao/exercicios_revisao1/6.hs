-- Construa uma função recursiva multiplic :: Int -> Int -> Int que faz a 
-- multiplicação de dois números inteiros não negativos através de somas sucessivas.

multiplic :: Int -> Int -> Int
multiplic 0 _ = 0
multiplic x y = y + multiplic (x-1) y