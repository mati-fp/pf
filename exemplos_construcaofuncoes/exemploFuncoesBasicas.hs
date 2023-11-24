-- quadrado de um numero (com inferencia de tipo)
quadrado x = x^2
-- :t quadrado
-- quadrado :: Num a => a -> a

-- quadrado de um numero (com tipagem explicita)
quadradov2 :: Integer -> Integer
quadradov2 x = x^2


-- divide como uma funcao de 'dois parametros'
-- d divide n se o resto da divisao de n por d for zero
divide :: (Integer, Integer) -> Bool
divide (d,n) = rem n d == 0

-- divide como uma funcao currificada
dividev2 :: Integer -> Integer -> Bool
dividev2 d n = rem n d == 0

-- o que acontece se tentarmos usar a funcao com um valor do tipo Int?
--dividev2 (4::Int) 16

-- divide como uma funcao sobrecarregada
-- com restricao de classe de tio
dividev3 :: Integral a => a -> a -> Bool
dividev3 d n = rem n d == 0


twice f x = f (f x)
double x = x*2

-- double (double 3)
-- double (3*2)
-- double 6
-- 6*2
-- 12
pair x y = (x,y)

second xs = head (tail xs)
