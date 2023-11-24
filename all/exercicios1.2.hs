-- 1. Escreva uma função "divide d n" que verifica se d é divisor de n.

divide :: Int -> Int -> Bool
divide d n = n `mod` d == 0

-- 2. Escreva uma função (fatorial, recursiva) para caluclar o fatorial de um número.
fatorial' :: Int -> Int
fatorial' 1 = 1
fatorial' x = x * fatorial' (x-1)

-- 3. Escreva uma função (palindrome) para verififcar se o parâmetro (uma lista) é um palíndromo.
palindroma :: Eq a => [a] -> Bool
palindroma xs = xs == reverse xs

-- 4. Escreva um função (soma) que soma os elementos de uma lista.
soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs

-- 5. Escreva uma função (reverso) que recebe uma lista e retorna a lista invertida.
reverso :: [a] -> [a]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]