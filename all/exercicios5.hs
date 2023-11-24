-- Como são conhecidas as funções de alta-ordem que retornam funções como resultado?
-- Funções de ordem superior

-- Expresse [f x | x <- xs, p x], utilizando as funções map e filter.
mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p xs = map f (filter p xs)

-- Redefina map f utilzando foldr
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f xs = foldr ((:).f) [] xs

-- Redefina filter p utilzando foldr
filterFoldr :: Foldable t => (a -> Bool) -> t a -> [a]
filterFoldr p xs = foldr (\x ys -> if (p x) then x : ys else ys) [] xs