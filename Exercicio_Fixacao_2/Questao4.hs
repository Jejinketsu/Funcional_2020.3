module Produtorio where

calc(x) = (2*(x^3) + 4*(x^2) + x)

produtorio(m, n)
    | m < n = produtorio(n, m)
    | m == n = 1 * calc(n)
    | otherwise = calc(n) * produtorio(n+1, m)

produtorioS(m, n, result)
    | m < n = produtorioS(n, m, result)
    | m == n = result * calc(n)
    | otherwise = produtorioS(n+1, m, result * calc(n))

main = do
        putStr "Digite M: "
        m <- getLine
        putStr "Digite N: "
        n <- getLine
        print(produtorio(read m :: Int, read n :: Int))
        print(produtorioS(read m :: Int, read n :: Int, 1))