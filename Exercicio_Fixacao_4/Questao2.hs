module Strings where

import Data.Char ( toLower )
import Data.Ord ( comparing )
import Data.List ( maximumBy, minimumBy )

tamanhoStrings :: [String] -> [Int]
tamanhoStrings lista = map (\x -> length x) lista

menorString :: [String] -> String
menorString lista = minimum lista

uneMinMaxString :: [String] -> String
uneMinMaxString lista = concat [minimumBy (comparing length) lista, maximumBy (comparing length) lista]

eh_Vogal :: Char -> Bool
eh_Vogal c
    | toLower(c) == 'a' || toLower(c) == 'e' || toLower(c) == 'i' || toLower(c) == 'o' || toLower(c) == 'u' = True
    | otherwise = False

soVogal :: [String] -> [String]
soVogal lista = filter (\x -> eh_Vogal(head x)) lista

checaEntrada :: Int -> Maybe Int
checaEntrada option
    | option >= 1 && option <= 4 = Just option
    | otherwise = Nothing

menu :: IO Int
menu = do
        print "Escolha uma das opcoes a seguir:"
        print "1. Retornar lista dos tamanhos das strings;"
        print "2. Retornar a menor string;"
        print "3. Retornar a unicao da menor e maior string;"
        print "4. Retornar as strings que comecam com vogal;"
        putStr "Digite sua escolha: "
        option <- getLine
        return (read option :: Int)

main :: IO ()
main = do
        putStr "Digite um lista no formato [string1,string2,..,stringn]: "
        l1 <- read <$> getLine :: IO [String]
        option <- menu
        case checaEntrada option of
            Nothing -> do 
                        print "Entrada invalida"
                        print "Tente novamente"
                        main
            Just 1 -> print(tamanhoStrings l1)
            Just 2 -> print(menorString l1)
            Just 3 -> print(uneMinMaxString l1)
            Just 4 -> print(soVogal l1)

        putStr "Deseja continua? 1. sim; 2. nao: "
        option2 <- getLine

        if (read option2 :: Int) == 1 then main
        else print "Fim do programa."