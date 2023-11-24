data Tree a = EmptyTree | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap f EmptyTree = EmptyTree
    fmap f (Node esq x dir) = Node (fmap f esq) (f x) (fmap f dir)

--let arv = EmptyTree
--fmap (+1) arv

--let arv = Node (Node EmptyTree 2 EmptyTree) 1 (Node EmptyTree 3 EmptyTree)
--fmap (+1) arv

--let arv = Node EmptyTree "raiz" (Node EmptyTree "folha" EmptyTree)
--fmap length arv