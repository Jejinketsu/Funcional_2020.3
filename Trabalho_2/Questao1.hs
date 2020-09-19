module Strings where

import Data.Char (toLower, toUpper, isDigit, isLetter)
import Data.Map (fromListWith, toList)
import Data.List (maximumBy)
import Data.Ord (comparing)

-- a --
contaLetra :: String -> Int
contaLetra str = length (toList (fromListWith (+) [(x,1) | x <- str]))

-- b --
ehVogal :: Char -> Bool
ehVogal c
    | toLower(c) == 'a' || toLower(c) == 'e' || toLower(c) == 'i' || toLower(c) == 'o' || toLower(c) == 'u' = True
    | otherwise = False

tipoChar :: Char -> String
tipoChar c
    | ehVogal c && c == toUpper c = "Vogal maiuscula"
    | ehVogal c && c == toLower c = "Vogal minuscula"
    | isLetter c && c == toUpper c = "Consoante maiuscula"
    | isLetter c && c == toLower c = "Consoante minuscula"
    | isDigit c = "Digito"
    | otherwise = "Outro"

tipoInicio :: [String] -> [String]
tipoInicio listaStrings = [tipoChar(c) | c:str <- listaStrings]

-- c --
contaVogal :: String -> Int
contaVogal [] = 0
contaVogal (c:r)
    | ehVogal(c) = 1 + contaVogal r
    | otherwise = contaVogal r

maisVogais :: [[Char]] -> ([Char], Int)
maisVogais listaStrings = maximumBy (comparing snd) (toList (fromListWith (+) [(str,contaVogal(str)) | str <- listaStrings]))