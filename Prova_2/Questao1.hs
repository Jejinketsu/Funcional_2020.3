module Strings where

import Data.Char ( toLower, isLetter )

-- lista1 <- read <$> getLine :: IO [String]

-- a --
ehVogal :: Char -> Bool
ehVogal c
    | caractere == 'a' || caractere == 'e' || caractere == 'i' || caractere == 'o' || caractere == 'u' = True
    | otherwise = False
        where 
            caractere = toLower(c)

ehConsoante :: Char -> Bool
ehConsoante c
    | isLetter(c) && not(ehVogal(c)) = True
    | otherwise = False

filtrarConsoante :: [String] -> [String]
filtrarConsoante [] = []
filtrarConsoante (c:r) 
    | ehConsoante(primeiro c) = c : filtrarConsoante(r)
    | otherwise = filtrarConsoante(r)

ultimo :: Eq p => [p] -> p
ultimo (c:r)
    | r == [] = c
    | otherwise = ultimo r

primeiro :: [a] -> a
primeiro (c:r) = c

maiorStringC :: [String] -> [String] -> String
maiorStringC lista1 lista2 = ultimo (filtrarConsoante (ordenaLista(lista1, lista2)))

-- b --
len :: (Eq a, Num p) => [a] -> p
len (c:r)
    | r == [] = 1
    | otherwise = 1 + len(r)

filtrarM7 :: [String] -> [String]
filtrarM7 [] = []
filtrarM7 (c:r) 
    | ehVogal(primeiro c) && (len (c)) < 7 = c : filtrarM7(r)
    | otherwise = filtrarM7(r) 

soVogalM7 :: [String] -> [String] -> [String]
soVogalM7 lista1 lista2 = (filtrarM7 lista1) ++ (filtrarM7 lista2)

-- c --
ordenaLista :: ([String],[String]) -> [String]
ordenaLista([],[]) = []
ordenaLista(l1,[]) = l1
ordenaLista([],l2) = l2
ordenaLista(c1:r1,c2:r2)
    | l1 == l2 = c1 : ordenaLista(r1,r2)
    | l1 < l2 = c1 : ordenaLista(r1,c2:r2)
    | otherwise = c2 : ordenaLista(c1:r1,r2)
        where
            l1 = length c1
            l2 = length c2

filtrarVogal :: [String] -> [String]
filtrarVogal [] = []
filtrarVogal (c:r) 
    | ehVogal(primeiro c) = c : filtrarVogal(r)
    | otherwise = filtrarVogal(r)

vogalEconsoante :: [String] -> [String] -> [String]
vogalEconsoante lista1 lista2 = ordenaLista(filtrarVogal lista1, filtrarConsoante lista2)