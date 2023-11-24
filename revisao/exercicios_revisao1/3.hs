--  Defina a função third :: [a] -> a que retorna o terceiro elemento de uma lista que 
-- contenha no mínimo três elementos usando:
-- a) As funções head e tail.
-- b) O indexador de listas !!.
-- c) O mecanismo de casamento de padrões

third :: [a] -> a
-- third xs = xs !! 2
--third xs = head (tail (tail xs)) -- third = head . tail . tail
third (_:_:x:_) = x