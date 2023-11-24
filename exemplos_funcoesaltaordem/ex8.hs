-- Composição de funções via operador .
-- f(g(x)) = (f . g)(x)

par :: Int -> Bool
par n = n `rem` 2 == 0

impar :: Int -> Bool
impar = not . par

-- Aplicação de função via operador $
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x
-- Cuidado! Tem menor precedência e é associativo à direita
-- sum (map sqrt [1..130])
-- sum $ map sqrt [1..130]
-- sum (filter (> 10) (map (*2) [2..10]))
-- sum $ filter (> 10) $ map (*2) [2..10]