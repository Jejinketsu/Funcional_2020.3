module StringOrdenada where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

ordenaLista([],[]) = []
ordenaLista(l1,[]) = l1
ordenaLista([],l2) = l2
ordenaLista(c1:r1,c2:r2)
    | c1 == c2 = c1 : ordenaLista(r1,r2)
    | c1 < c2 = c1 : ordenaLista(r1,c2:r2)
    | otherwise = c2 : ordenaLista(c1:r1,r2)

main = do
    hSetBuffering stdout NoBuffering
    putStr "Digite uma string ordenada: "
    s1 <- getLine
    putStr "Digite uma string ordenada: "
    s2 <- getLine
    putStr "Unisao ordenada das strings: "
    print(ordenaLista(s1,s2))