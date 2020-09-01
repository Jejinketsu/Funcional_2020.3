module Perfeito where

somaDiv(x, cont)
    | cont == x = cont
    | mod x cont == 0 = somaDiv(x, cont+1) + cont
    | otherwise = somaDiv(x, cont+1)

principal(x)
    |somaDiv(x, 1) == soma = show(x) ++ " eh perfeito, pois " ++ show(somaDiv(x, 1)) ++  " = " ++ show(x) ++ " * 2" ++ " , ou seja s(N) = 2*N"
    |somaDiv(x, 1) > soma = show(x) ++ " eh abundante, pois " ++ show(somaDiv(x, 1)) ++ " > " ++ show(x) ++ " * 2" ++ " , ou seja s(N) > 2*N"
    |somaDiv(x, 1) < soma = show(x) ++ " eh deficiente, pois " ++ show(somaDiv(x, 1)) ++ " < " ++ show(x) ++ " * 2" ++ " , ou seja s(N) < 2*N"
        where
            soma = x * 2

main = do
        putStr "|----- Para finalizar digite 0 -----|"
        putStr "\n"
        putStr "Digite um numero >> "
        n <- getLine
        if (read n) > 0
            then do
                print(principal(read n))
                putStr "\n"
                main
        else
            print "Programa finalizado"
    