--class (Functor f) => Applicative f where  
--    pure :: a -> f a  
-- pure recebe um valor de qualquer tipo e retorna um funtor aplicativo sobre o valor
-- pure toma um valor e o coloca dentro de um contexto computacional puro
--    (<*>) :: f (a -> b) -> f a -> f b
-- <*> é um operador que representa uma generalização de fmap
-- <*> toma um funtor que possui uma função e um outro funtor e resulta na aplicação da função "extraída" do primeiro funtor sobre o segundo

-- Exemplo de Maybe
-- pure (+1) <*> Just 1
-- pure (+1) <*> Nothing
-- Nothing <*> Just 1
-- Just (+1) <*> Just 1
-- pure (+) <*> Just 1 <*> Just 2
-- pure (+) <*> Just 1 <*> Nothing
-- pure (+) <*> Nothing <*> Just 1 

-- Exemplo de []
-- pure (+1) <*> [1,2,3]
-- [(+1),(*2)] <*> [1,2,3]
-- pure (+) <*> [1] <*> [2]
-- pure (+) <*> [1,2] <*> [3,4]
-- [(+),(*)] <*> [1,2] <*> [3,4]