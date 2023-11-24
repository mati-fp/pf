-- Usando recursão e a função add, defina uma função que multiplica dois números naturais.
data Nat = Zero | Succ Nat deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

multN :: Nat -> Nat -> Nat
multN Zero _ = Zero
multN _ Zero = Zero
multN (Succ Zero) n = n
multN n (Succ Zero) = n
multN (Succ m) n = add n (multN m n)

-- Defina uma função folde para expressões e mostre alguns exemplos de uso.
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr deriving (Show, Eq)

folde :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
folde f _ _ (Lit n) = f n
folde f g h (Add e1 e2) = g (folde f g h e1) (folde f g h e2)
folde f g h (Sub e1 e2) = h (folde f g h e1) (folde f g h e2)

-- Exemplos de uso:
-- folde (\x -> x) (\x y -> x + y) (\x y -> x - y) (Lit 3)
-- folde (\x -> x) (\x y -> x + y) (\x y -> x - y) (Add (Lit 3) (Lit 4))
-- folde (\x -> x) (\x y -> x + y) (\x y -> x - y) (Sub (Lit 3) (Lit 4))

-- Defina um novo tipo Tree a de árvores binárias construídas a partir de valores Leaf 
-- do tipo a utilizando um construtor Node que toma duas árvores binárias como parâmetro.

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)
-- Exemplo de criação de árvore binária balanceada de altura 3:
-- let btree = (Node (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))) (Node (Node (Leaf 5) (Leaf 6)) (Node (Leaf 7) (Leaf 8))))

-- Dizemos que tal árvore é balanceada se o número de folhas nas subárvores da esquerda e 
-- direita de cada nó difere no máximo de 1. Sabe-se que folhas são trivialmente balanceadas. 
-- Defina a seguinte função:

balanced' :: Tree a -> Bool
balanced' (Leaf _) = True
balanced' (Node l r) = (countLeaf l - countLeaf r) <= 1 && balanced' l && balanced' r

countLeaf :: Tree a -> Int
countLeaf (Leaf _) = 1
countLeaf (Node l r) = countLeaf l + countLeaf r

-- Crie uma função de conte o número de nodos de uma árvore
countNodes :: Tree a -> Int
countNodes (Leaf _) = 1
countNodes (Node l r) = 1 + countNodes l + countNodes r


