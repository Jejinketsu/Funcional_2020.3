module Fatorial where

fat(x)
    | x == 0 = 1
    | otherwise = x * fat(x-1)

main = do
    putStr "Digite um numero: "
    x <- getLine
    print(fat(read x))