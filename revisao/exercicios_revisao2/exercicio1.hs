-- Escreva um programa em Haskell que lê do console um valor inteiro e informa se ele é ou 
-- não um número Armstrong. Um número Armstrong é um número cuja soma de seus dígitos 
-- elevados à quantidade de dígitos do número resulta no próprio número. Por exemplo:
-- • 9 é um número Armstrong, pois 9¹ = 9.
-- • 10 não é um número Armstrong, pois 1² + 0² ≠ 10.
-- • 153 é um número Armstrong, pois 1³ + 5³ + 3³ = 153.

digitos :: Int -> [Int]
digitos 0 = []
digitos x = digitos (x `div` 10) ++ [mod x 10]

armstrong :: Int -> Bool
armstrong x = sum [d^n | d <- ds ] == x
                where ds = digitos x
                      n = length (digitos x)

valorNumerico :: IO Int  -- Correção aplicada aqui
valorNumerico = readLn

main = do
    putStrLn "Digite um número:"
    x <- valorNumerico
    if armstrong x then putStrLn "É um número de Armstrong"
    else putStrLn "Não é um número de Armstrong"

