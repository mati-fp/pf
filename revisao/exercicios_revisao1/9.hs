-- Sem olhar para a implementação disponível no prelude de Haskell, defina suas próprias 
-- versões para as seguintes funções:
-- a) Função de alta ordem curry que converte uma função sobre pares em uma versão 
-- currificada da função.
-- b) Função de alta ordem uncurry que converte uma função currificada sobre dois argumentos 
-- e uma função sobre pares.

-- a) Função de alta ordem curry
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f = \x y -> f (x, y)

-- b) Função de alta ordem uncurry
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f = \(x, y) -> f x y