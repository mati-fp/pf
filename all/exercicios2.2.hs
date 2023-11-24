-- 1.Considere a função safetail que te o mesmo comportamento que tail, exceto que safetail mapeia a lista vazia para a lista vazia,
-- enquanto que tail gera um erro. Defina safetail usando:
-- a. uma equação de condição
-- b. equações guardadas
-- c. casamento de padrões

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs | null xs = []
            | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (x:xs) = xs

-- 2. Dê três definições diferentes para o operador lógico OU (||) usando casamento de padrões.
-- a.
ou1 :: Bool -> Bool -> Bool
ou1 False False = False
ou1 _ _ = True
-- b.
ou2 :: Bool -> Bool -> Bool
ou2 True _ = True
ou2 _ True = True
ou2 _ _ = False
-- c.
ou3 :: Bool -> Bool -> Bool
ou3 False b = b
ou3 True _ = True

-- 3. Redefina a seguinte versão de (&&) usando condicionais ao invés de padrões:
-- True && True = True
-- _ && _ = False
and :: Bool -> Bool -> Bool
and a b = if not (a || b) then False else True

--Pega o valor que é recebido "n" e cria uma lista que vai ser criada usando a função com valores de 0 até n-1
criaLista :: Int -> [Int]
criaLista n = map f [0..n-1]
        where
            f x = x*2 + 1

-- Simplificando a função acima com lambda (anonima)
odds2 :: Int -> [Int]
odds2 n = map (\x -> x*2 + 1) [0..n-1]

-- Função abaixo recebe uma lista e retorna a mesma apenas com números ímpares
impares :: [Int] -> [Int]
impares xs = [ x | x <- xs, mod x 2 /= 0]

-- Simplificando a função acima com lambda (anonima)
listaImparesAnon :: [Int] -> [Int]
listaImparesAnon xs = filter (\x -> mod x 2 /= 0) xs