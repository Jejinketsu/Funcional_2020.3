module Strings where

import Data.Char (toLower, toUpper, isDigit, isLetter)
import Data.Map (fromListWith, toList)
import Data.List (maximumBy)
import Data.Ord (comparing)


-- a --
contaLetra :: String -> Int
contaLetra str = length ( toList (fromListWith (+) [(toLower(x),1) | x <- str]))

letrasDiferenteString :: [String] -> [Int]
letrasDiferenteString lista =  [contaLetra(str) | str <- lista]

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

maiorNumeroVogal :: ([String]) -> [String]
maiorNumeroVogal(lista) = filter(\x -> contaVogal(x) > 0 && contaVogal(x) == (maximum(map(\y -> contaVogal(y)) lista)) ) lista

verificaRetorno :: [String] -> Maybe [String]
verificaRetorno(lista)
    |length(lista) > 0 = Just lista
    |otherwise = Nothing



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
    
main :: IO()
main = do
        putStr "Entre com uma lista de Strings no seguinte formato: [string1, string2, string3] \n"
        putStr ">> "
        lista <- read <$> getLine :: IO [String]
        aplicacao lista

aplicacao :: [String] -> IO()
aplicacao lista = do
                    opcao <- menu
                    case checaEntrada opcao 1 4 of
                        Nothing -> do 
                                print("Opcao invalida, tente novamente")
                                aplicacao lista
                        Just 1 -> do
                                print(letrasDiferenteString lista)
                                aplicacao lista
                        Just 2 -> do
                                print(tipoInicio lista)
                                aplicacao lista
                        Just 3 -> do
                                case verificaRetorno(maiorNumeroVogal(lista)) of
                                    Just lista -> do
                                            putStr "Strings com maior numero de vogais \n"
                                            print(lista)
                                            aplicacao lista
                                    Nothing -> do
                                                putStr("Nao ha strings com a presenca de vogais")
                                                aplicacao lista