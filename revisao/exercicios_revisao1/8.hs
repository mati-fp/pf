-- Sem olhar para a implementação disponível no prelude de Haskell, defina suas próprias 
-- versões para as seguintes funções:
-- a) Decide se todos os elementos de uma lista satisfazem um predicado.
-- all :: (a -> Bool) -> [a] -> Bool
-- b) Decide se pelo menos um dos elementos de uma lista satisfaz um predicado.
-- any :: (a -> Bool) -> [a] -> Bool
-- c) Seleciona os elementos de uma lista enquanto eles satisfazem um predicado.
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- d) Remove os elementos de uma lista enquanto eles satisfazem um predicado.
-- dropWhile :: (a -> Bool) -> [a] -> [a]

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and [p x | x <- xs]

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or [p x | x <- xs]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
    | p x = dropWhile' p xs
    | otherwise = x:xs