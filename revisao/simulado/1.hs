import Data.Functor.Contravariant ()
-- Questão 2
fibs :: [Integer]
fibs = 0:1:[x + y | (x,y) <- zip fibs (tail fibs)]

-- Questão 3
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
-- leaves :: Tree a -> Int
-- leaves (Leaf _) = 1
-- leaves (Node l r) = leaves l + leaves r

-- balanced :: Tree a -> Bool
-- balanced (Leaf _) = True
-- balanced (Node l r) = abs(leaves l - leaves r) <= 1 && balanced l && balanced r

-- -- Questão 4
-- halve :: [a] -> ([a], [a])
-- halve xs = splitAt (length xs `div` 2) xs

-- balance :: [a] -> Tree a
-- balance [x] = Leaf x
-- balance xs = Node (balance ys) (balance zs)
--             where (ys, zs) = halve xs

-- Questão 5
-- data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

-- instance Functor Tree where 
--     fmap :: (a -> b) -> Tree a -> Tree b
--     fmap g Leaf = Leaf
--     fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

-- Questão 6
data Expr = Val Int | App Op Expr Expr
data Op = Add | Mul

eval :: Expr -> Int
eval (Val n) = n
eval (App Add l r) = eval l + eval r
eval (App Mul l r) = eval l * eval r

values :: Expr -> [Int]
values (Val n) = [n]
values (App Add l r) = values l ++ values r
values (App Mul l r) = values l ++ values r

-- Questão 7
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

repeatT :: a -> Tree a
repeatT x = Node (repeatT x) x (repeatT x)

takeT :: Int -> Tree a -> Tree a
takeT 0 _ = Leaf
takeT _ Leaf = Leaf
takeT n (Node l x r) = Node (takeT (n - 1) l) x (takeT (n - 1) r)

replicateT :: Int -> a -> Tree a
replicateT n = takeT n . repeatT