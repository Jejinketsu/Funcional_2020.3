module Perfeito where

somaDiv(x, cont)
    | cont == x = cont
    | mod x cont == 0 = somaDiv(x, cont+1) + cont
    | otherwise = somaDiv(x, cont+1)

principal(x)
    |somaDiv(x, 1) == soma = show(x) ++ " eh perfeito, pois " ++ show(x) ++ " * 2 = " ++ show(somaDiv(x, 1)) ++ " , ou seja s(N) = 2*N"
    |somaDiv(x, 1) > soma = show(x) ++ " eh abundante, pois " ++ show(x) ++ " * 2 < " ++ show(somaDiv(x, 1)) ++ " , ou seja s(N) > 2*N"
    |somaDiv(x, 1) < soma = show(x) ++ " eh deficiente, pois " ++ show(x) ++ " * 2 > " ++ show(somaDiv(x, 1)) ++ " , ou seja s(N) < 2*N"
        where
            soma = x * 2

main = do
        putStr "Digite um numero >> "
        n <- getLine
        print(principal(read n))
    