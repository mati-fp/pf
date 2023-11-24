--  Defina uma função recursiva euclid :: Int -> Int -> Int que implementa o 
-- Algoritmo de Euclides para calcular o máximo divisor comum (MDC) entre dois números 
-- inteiros não negativos.

-- Para implementar o Algoritmo de Euclides em Haskell, você pode usar uma função recursiva.
-- A função deve ter dois argumentos, `a` e `b`.
-- Se `b` é 0, então `a` é o MDC e deve ser retornado.
-- Se `b` não é 0, então chame a função recursivamente com `b` e o resto da divisão de `a` por `b`.

euclid :: Int -> Int -> Int
euclid a 0 = a
euclid a b = euclid a (a `mod` b)