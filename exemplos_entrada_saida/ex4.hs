invertePalavras :: String -> String
invertePalavras = unwords . map reverse . words

main = do
    linha <- getLine
    if null linha
        then return ()
        else do
            putStrLn (invertePalavras linha)
            main
-- runhaskell ex4.hs