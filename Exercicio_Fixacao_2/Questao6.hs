module Somatorio where

calc(x, y) = (x^2) + (y^2)

somatorioY(n, x, y)
    | y == n = 0
    | otherwise = calc(x, y) + somatorioY(n, x, y+1)

somatorioX(m, n, x)
    | x == n = 0
    | otherwise = somatorioY(n, x, 1) + somatorioX(m, n, x+1)

main = do
        putStr "Digite M: "
        m <- getLine
        putStr "Digite N: "
        n <- getLine
        print(somatorioX(read m :: Int, read n :: Int, 2))