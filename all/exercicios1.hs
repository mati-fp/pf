-- 1. Escreva uma função "divide d n" que verifica se d é divisor de n.
divide :: Integral a => (a,a) -> Bool
divide(d,n) = mod d n  == 0


-- 2. Escreva uma função (fatoriall, recursiva) para caluclar o fatorial de um número.
fatorial :: Integral a => a -> a
fatorial 0 = 1
fatorial n = n * fatorial (n-1)



-- 3. Escreva uma função (palindrome) para verififcar se o parâmetro (uma lista) é um palíndromo.
palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs


-- 4. Escreva um função (soma) que soma os elementos de uma lista.
soma :: Num a => [a] -> a
soma [] = 0 -- base da recursão
soma (x:xs) = x + soma xs -- soma da recursão


-- 5. Escreva uma função (reverso) que recebe uma lista e retorna a lista invertida.
reverso :: [a] -> [a]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

-- (x:xs) significa pega o head e o tail da lista

-- reverso [1,2,3]
-- reverso [2,3] ++ [1]
-- reverso [3] ++ [2] ++ [1]
-- reverso [] ++ [3] ++ [2] ++ [1]
-- [] ++ [3] ++ [2] ++ [1]
-- [3,2,1]