mdc :: Int -> Int -> Int
-- equação de condição
-- mdc a b = if b == 0 then a 
--           else mdc b (mod a b)

-- equação guardadas
-- mdc a b | b == 0 = a
--         | otherwise = mdc b (a `mod` b)

-- casamento de padrões (patterns matching)
mdc a 0 = a
mdc a b = mdc b (mod a b)