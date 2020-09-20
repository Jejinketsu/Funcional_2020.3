module Strings where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Char

maiorString([], maior) = maior
maiorString(c:r, maior)
    | length c > length maior = maiorString(r, c)
    | otherwise = maiorString(r, maior)

stringMenorQ6(strings) = [s | s <- strings, length s < 6]

primeiraMaiuscula([]) = 0
primeiraMaiuscula(c:r)
    | isUpper(c!!0) = 1 + primeiraMaiuscula(r)
    | otherwise = primeiraMaiuscula(r)

soMaiuscula(strings) = [s | s <- strings, isUpper(s!!0)]

listTeste = ["qual","A","maior","Palavra","desse","Role","paralelepipedo"]
-- ["picos", "barra", "teresina", "novo oriente", "inhuma", "Valenca"]
-- ["qual","A","maior","Palavra","desse","Role","paralelepipedo"]

main = do
    putStr "Maior String: "
    print(maiorString(listTeste, ""))
    putStr "String < 6: "
    print(stringMenorQ6(listTeste))
    putStr "Com letra maiuscula: "
    print(primeiraMaiuscula(listTeste))
    putStr "So as com letra maiuscula: "
    print(soMaiuscula(listTeste))