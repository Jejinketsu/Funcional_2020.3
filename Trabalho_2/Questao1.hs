module ListaOrdenada where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

ordenaLista([],[]) = []
ordenaLista(c1:r1,[])
    | even c1 = c1 : ordenaLista(r1,[])
    | otherwise = ordenaLista(r1,[])
ordenaLista([],c2:r2)
    | even c2 = c2 : ordenaLista([],r2)
    | otherwise = ordenaLista([],r2)
ordenaLista(c1:r1,c2:r2)
    | c1 == c2 && even c1 = c1 : ordenaLista(r1,r2)
    | c1 < c2 && even c1 = c1 : ordenaLista(r1,c2:r2)
    | c2 < c1 && even c2 = c2 : ordenaLista(c1:r1,r2)
    | c1 < c2 = ordenaLista(r1,c2:r2)
    | otherwise = ordenaLista(c1:r1,r2)

main = print(ordenaLista([2,3,4,14,18,20,32],[1,2,5,16,17,30]))