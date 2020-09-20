module Fatorial where

fatorial(n)
    |n == 0 = 1
    |otherwise = n * fatorial(n-1)
    
calc(n, x) = x * ((fatorial(n) * 5) / (fatorial(n-5)*2))

produtorio_CP(m, n)
    |m < 5 = 0
    |n == m = calc(n, 1)
    |otherwise = calc(n, 1) * produtorio_CP(m, n+1)

produtorio_SP(m, n, x)
    |m < 5 = 0
    |n == m = calc(n, x)
    |otherwise = produtorio_SP(m, n+1, calc(n, x))


lerEntrada = do
                putStr "Digite um numero, tal que n >= 5 : "
                x <- getLine        
                if (read x) < 5
                    then do
                        putStr "Numero invalido \n"
                        lerEntrada
                else return x

verificaFuncao(m, op)
    |op == 1 = produtorio_CP(m, 5)
    |op == 2 = produtorio_SP(m, 5, 1)
    |otherwise = error "A opcao informada eh invalida"


main :: IO()
main = do
        print "Escolha o tipo de funcao recursiva pelo numero" 
        putStr "\n"
        print "| 1 - Recursao com Pendencia |"
        print "| 2 - Recursao sem Pendencia |"
        putStr "\n Digite uma opcao >> "
        opcao <- getLine
        m <- lerEntrada
        putStr "O resultado da operacao eh: "
        putStr (show(verificaFuncao(read m, read opcao)))
        putStr "\n"