module Strings where

import Data.Char (toLower, toUpper, isDigit, isLetter)
import Data.Map (fromListWith, toList)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Char


buscaCaractere :: (Int, Char, String) -> Int
buscaCaractere(cont, x, []) = cont
buscaCaractere(cont, x, c:r)
    |toLower(x) == toLower(c) = buscaCaractere(cont+1, x, r)
    |otherwise = buscaCaractere(cont, x, r)

nCaracteres :: (Int, String, String) -> Int
nCaracteres(i,[], copiaString) = i
nCaracteres(i, c:r,copiaString)
    |buscaCaractere(0, c, copiaString) == 1 = nCaracteres(i+1, r, copiaString)
    |otherwise = nCaracteres(i, r, copiaString)

caSemRepetir :: ([String]) -> [Int]
caSemRepetir(lista) = map(\x -> nCaracteres(0, x, x)) lista
-- a --
contaLetra :: String -> Int
contaLetra str = length (toList (fromListWith (+) [(x,1) | x <- str]))

-- b --
ehVogal :: Char -> Bool
ehVogal c
    | caractere == 'a' || caractere == 'e' || caractere == 'i' || caractere == 'o' || caractere == 'u' = True
    | otherwise = False
        where 
            caractere = toLower(c)

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

checaEntrada :: Int -> Int -> Int -> Maybe Int
checaEntrada option min max
    | option >= min && option <= max = Just option
    | otherwise = Nothing  

menu :: IO Int
menu = do
    putStr "\n"
        print("[*------------------------------------------------*]")
        print "| 1 - Numeros caracteres sem repetir               |"
        print "| 2 - Tipos de caracteres iniciais das listas      |"
        print "| 3 - Strings com maior numero de vogais           |"
        print("[*------------------------------------------------*]")
        putStr "\nDigite uma opcao >> "
        op <- getLine
        return (read op :: Int)
    
main = do
        putStr "Entre com uma lista de Strings no seguinte formato: [string1, string2, string3]: "
        lista <- read <$> getLine :: IO [String]
        opcao <- menu
        case checaEntrada opcao 1 4 of
            Nothing -> do 
                    print("Opcao invalida, tente novamente")
                    main
            Just 1 -> do
                    print([contaLetra(str) | str <- lista])
                    main
            Just 2 -> do
                    print(tipoInicio lista)
                    main
            Just 3 -> do
                    print(maisVogais lista)
                    main
