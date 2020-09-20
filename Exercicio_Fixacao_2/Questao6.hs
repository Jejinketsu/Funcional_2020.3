module Somatorio where

calc(x, y) = (x^2) + (y^3)

somatorioY(n, x, y)
    | n < 1 = 0
    | y == n = calc(x, y)
    | otherwise = calc(x, y) + somatorioY(n, x, y+1)

somatorioX(m, n, x)
    | m < 2 = 0
    | x == m = somatorioY(n, x, 1)
    | otherwise = somatorioY(n, x, 1) + somatorioX(m, n, x+1)

main = do
        putStr "Digite M: "
        m <- getLine
        putStr "Digite N: "
        n <- getLine
        print(somatorioX(read m :: Int, read n :: Int, 2))