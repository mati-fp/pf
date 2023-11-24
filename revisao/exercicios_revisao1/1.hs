-- Mostre que a função currificada a seguir pode ser formalizada através de expressões 
-- lambda:
mult :: Int -> Int -> Int -> Int
-- mult x y z = x * y * z
mult = \x -> \y -> \z -> x * y * z