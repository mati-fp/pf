concat' :: [a] -> [a] -> [a]
concat' [] ys = ys
concat' (x:xs) ys = x : concat' xs ys
