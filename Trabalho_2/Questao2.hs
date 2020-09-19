module Grupos where

-- a --
ordenaLista :: ([Int],[Int]) -> [Int]
ordenaLista([],[]) = []
ordenaLista(l1,[]) = l1
ordenaLista([],l2) = l2
ordenaLista(c1:r1,c2:r2)
    | c1 == c2 = c1 : ordenaLista(r1,r2)
    | c1 < c2 = c1 : ordenaLista(r1,c2:r2)
    | otherwise = c2 : ordenaLista(c1:r1,r2)

subtrairGrupo :: [Int] -> [Int] -> [Int]
subtrairGrupo [] grupoB = []
subtrairGrupo (c:r) grupoB
    | not(elem c grupoB) = c : subtrairGrupo r grupoB
    | otherwise = subtrairGrupo r grupoB

uniaoOrdenada :: [Int] -> [Int] -> [Int]
uniaoOrdenada grupoA grupoB = ordenaLista((subtrairGrupo grupoA grupoB), (subtrairGrupo grupoB grupoA))

-- b --
listaDeSomas :: [Int] -> [Int] -> [Int]
listaDeSomas grupoA grupoB = [(x^2)+(y^2) | x <- grupoA, y <- grupoB, (x^2)+(y^2) > (((grupoA!!0)^3)+((grupoB!!0)^3))] 