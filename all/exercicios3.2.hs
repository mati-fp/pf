-- Uma tripla (x,y,z) de inteiros positivos é 
-- chamada pitagórica se x2 + y2 = z2. 
-- Usando list comprehension, defina a função
pyths :: Int -> [(Int,Int,Int)]
-- que mapeia um inteiro n para todas as triplas
-- com componentes em [1..n]. Por exemplo:
-- > pyths 5
-- [(3,4,5),(4,3,5)]
pyths n = [(x,y,z) | x <- [0..n], y <- [0..n], z <- [0..n], x*x + y*y == z*z]


-- Um inteiro positivo é dito perfeito se ele for 
-- igual a soma de todos os seus divisores, com 
-- exceção do próprio número. Usando list 
-- comprehension, defina a função
perfects :: Int-> [Int]
-- que retorna a lista de todos números perfeitos
-- até um limite dado. Por exemplo:
-- > perfects 500
-- [6,28,496]
perfects n = [x | x <- [1..n], f x]
                where f x = sum [ y | y <- [1..(x-1)], mod x y == 0] == x


-- O produto escalar de duas listas de inteiros xs e 
-- ys de tamanho n é dado pela soma dos 
-- produtos dos elementos correspondentes:

-- Usando list comprehension, defina a função
-- que retorna o produto escalar de duas listas.
prodEscalar :: [Int] -> [Int] -> Int
prodEscalar xs ys = sum [x*y | (x,y) <- zip xs ys]

