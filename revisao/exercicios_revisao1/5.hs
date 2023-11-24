-- Considere a função padrão replicate :: Int -> a -> [a] que produz uma lista de 
-- valores repetidos. Construa sua própria versão dessa função usando list comprehension. 
-- Por exemplo:
-- > replicate 3 True
-- [True,True,True]

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x