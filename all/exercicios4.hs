and' :: [Bool] -> Bool
and' [] = True
and' (x:xs)
    | x == False = False
    | otherwise = and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x


enesimo :: [a] -> Int -> a
enesimo [] _ = error "Lista vazia"
enesimo (x:xs) n 
    | n <= 0 = x
    | otherwise = enesimo xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) | e == x = True
            | otherwise = elem' e xs

-- Recebe as duas ordenadas
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | y <= x = y : merge (x:xs) ys

-- Pode receber as duas não ordenadas
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort(left)) (msort(right))
    where
        left = take (length xs `div` 2) xs
        right = drop (length xs `div` 2) xs
