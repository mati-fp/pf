pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z]

perfect :: Int -> Bool
perfect n = sum [x | x <- [1..n-1], n `mod` x == 0] == n
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]


scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x*y | (x,y) <- zip xs ys ]