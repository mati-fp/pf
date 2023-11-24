-- Defina o tipo Tree do exercício anterior como um membro da classe Functor. Complete a 
-- seguinte definição:
-- instance Functor Tree where
-- -- fmap :: (a -> b) -> Tree a -> Tree b

-- Não cai na prova:

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)
instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)
