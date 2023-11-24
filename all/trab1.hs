import GHC.Float (int2Double)
-- 1. Definir uma função recursiva que recebe um número binário (interpretado como número inteiro sem sinal) e
-- retorna o valor equivalente em decimal. 𝑏𝑖𝑛2𝑑𝑒𝑐 ∷ [𝐼𝑛𝑡] → 𝐼𝑛𝑡
bin2dec :: [Int] -> Int
bin2dec [] = 0
bin2dec (x:xs) = x * 2^(length xs) + bin2dec xs

-- bin2dec [1,1,0,1,0,1,0,1]
-- 213

-- 2. Definir uma função recursiva que recebe um número decimal inteiro não-negativo, um número de bits
-- desejado e retorna o valor equivalente em binário (interpretado como número inteiro sem sinal) com o
-- número de bits informado. Por exemplo, 𝑑𝑒𝑐2𝑏𝑖𝑛 2 8 deve retornar [0,0,0,0,0,0,1,0]. 𝑑𝑒𝑐2𝑏𝑖𝑛 ∷ 𝐼𝑛𝑡 → 𝐼𝑛𝑡 → [𝐼𝑛𝑡]
dec2bin :: Int -> Int -> [Int]
dec2bin _ 0 = []
dec2bin x n 
    | x < 0 = error "Valor negativo não é permitido"
    | x >= (2^n) = error "Valor em binário não cabe no tamanho desejado"
    | otherwise = dec2bin (x `div` 2) (n-1) ++ [x `mod` 2]

-- dec2bin 100 8
-- [0,1,1,0,0,1,0,0]

-- 3. Definir uma função recursiva que recebe um número binário na representação de complemento de dois e
-- retorna o valor equivalente em decimal inteiro. 𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙2𝑑𝑒𝑐 ∷ [𝐼𝑛𝑡] → 𝐼𝑛𝑡
bincompl2dec :: [Int] -> Int
bincompl2dec xs 
    | head xs == 1 = (bincompl2dec' xs) * (-1)
    | otherwise = bin2dec xs

bincompl2dec' :: [Int] -> Int
bincompl2dec' [] = 0
bincompl2dec' (x:xs) = x * 2^(length xs) - bin2dec xs

-- bincompl2dec [1,0,0,1,1,1,1,0]
--  -98

-- 4. Definir uma função recursiva que recebe um número decimal inteiro, um número de bits desejado e retorna
-- o valor equivalente em binário na representação de complemento de dois com o número de bits informado.
-- Por exemplo, 𝑑𝑒𝑐2𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙 (−2) 8 deve retornar [1,1,1,1,1,1,1,0] 𝑑𝑒𝑐2𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙 ∷ 𝐼𝑛𝑡 → 𝐼𝑛𝑡 → [𝐼𝑛𝑡]
dec2bincompl :: Int -> Int -> [Int]
dec2bincompl _ 0 = []
dec2bincompl x n 
    | x > -1 = dec2bin x n
    | otherwise = dec2bincompl' (map (\bit -> if bit==0 then 1 else 0) (dec2bin (x*(-1)) n))

dec2bincompl' :: [Int] -> [Int]
dec2bincompl' [] = []
dec2bincompl' xs
    | last xs == 0 = init xs ++ [1]  -- Se o último bit é 0, mude-o para 1
    | otherwise = dec2bincompl' (init xs) ++ [0]  -- Se o último bit é 1, mude-o para 0 e adicione 1 ao resto

-- dec2bincompl (-100) 9
-- [1,1,0,0,1,1,1,0,0]

-- 5. Definir uma função recursiva que recebe um número fracionário decimal por parâmetro e devolve um
-- número binário de ponto fixo de 32 bits. O número binário de ponto fixo dever ser representado por uma
-- tupla com dois números binários tal que a parte inteira deve estar na representação de complemento de
-- dois com 16 bits e a parte fracionária deve estar na representação de binário fracionado com 16 bits. Você
-- deve definir uma forma adequada de representar o resultado caso o número decimal estoure a
-- representação. Por exemplo, 𝑓𝑟𝑎𝑐2𝑏𝑖𝑛 (−8.5) deve retornar
-- ([1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0], [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]). 𝑓𝑟𝑎𝑐2𝑏𝑖𝑛 ∷ 𝐷𝑜𝑢𝑏𝑙𝑒 → ([𝐼𝑛𝑡], [𝐼𝑛𝑡])
frac2bin :: Double -> ([Int], [Int])
frac2bin x 
    | x > 2^16 = error "Valor maior do que pode ser representado pela função"
    | otherwise = frac2bin' (properFraction x)

frac2bin' :: (Int, Double) -> ([Int], [Int])
frac2bin' (x, y) = (dec2bincompl x 16, frac2bin'' y 16) 

frac2bin'' :: Double -> Int -> [Int]
frac2bin'' _ 0 = []
frac2bin'' 0.0 n = replicate n 0
frac2bin'' x n 
            | 1 >= (x*2) = 1 : frac2bin'' ((x*2) - fromIntegral (floor (x*2))) (n-1)
            | otherwise = 0 : frac2bin'' ((x*2) - fromIntegral (floor (x*2))) (n-1)

-- frac2bin (-328.10)
-- ([1,1,1,1,1,1,1,0,1,0,1,1,1,0,0,0],[1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1])


-- 6. Definir uma função recursiva que recebe uma tupla com dois números binários representando,
-- respectivamente, a parte inteira (na representação de complemento de dois com 16 bits) e a parte
-- fracionária (na representação de binário fracionado com 16 bits) de um número binário de ponto fixo com 32
-- bits, e retorna o correspondente valor fracionário decimal. Por exemplo,
-- 𝑏𝑖𝑛2𝑓𝑟𝑎𝑐 ([0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0], [1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]) deve retornar
-- 16392.625. 𝑏𝑖𝑛2𝑓𝑟𝑎𝑐 ∷ ([𝐼𝑛𝑡], [𝐼𝑛𝑡]) → 𝐷𝑜𝑢𝑏𝑙𝑒
bin2frac :: ([Int], [Int]) -> Double
bin2frac ([], []) = error "Nenhum valor passado na função"
bin2frac (_, []) = error "Nenhum valor fracionário passado na função"
bin2frac ([], _) = error "Nenhum valor inteiro passado na função"
bin2frac (xs, ys) 
            | head xs == 1 = int2Double (bincompl2dec xs) - bin2frac' (reverse ys)
            | otherwise = int2Double (bincompl2dec xs) + bin2frac' (reverse ys)

bin2frac' :: [Int] -> Double
bin2frac' [] = 0
bin2frac' (x:xs)
            | x == 1 = (1/2^((length xs)+1)*1.0) + bin2frac' xs
            | otherwise = 0.0 + bin2frac' xs

-- bin2frac ([1,1,0,0,1,1,1,1,1,1,1,1,1,0,0,1], [1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0])
--  -12295.875