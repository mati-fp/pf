double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]
average ns = sum ns `div` length ns -- mesma coisa que div (sum ns) (length ns)

getLastFromList ns = ns !! ((length ns) - 1)
-- ou
getLastFromList2 ns = head (reverse ns)

removeLastFromList ns = take ((length ns) -1) ns
-- ou
removeLastFromList2 ns = reverse (drop 1 (reverse ns))
