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

{-
subtrairGrupo :: [Int] -> [Int] -> [Int]
subtrairGrupo [] grupoB = []
subtrairGrupo (c:r) grupoB
    | not(elem c grupoB) = c : subtrairGrupo r grupoB
    | otherwise = subtrairGrupo r grupoB
-}

subtrairGrupo :: [Int] -> [Int] -> [Int]
subtrairGrupo lista1 lista2 = [a | a <- lista1, not(elem a lista2)]

uniaoOrdenada :: [Int] -> [Int] -> [Int]
uniaoOrdenada grupoA grupoB = ordenaLista((subtrairGrupo grupoA grupoB), (subtrairGrupo grupoB grupoA))

-- b --
listaDeSomas :: [Int] -> [Int] -> [Int]
listaDeSomas grupoA grupoB = [x | x <- (map (\x -> x^2) (grupoA ++ grupoB)), x > (((grupoA!!0)^3)+((grupoB!!0)^3))]


checaEntrada :: Int -> Int -> Int -> Maybe Int
checaEntrada option min max
    | option >= min && option <= max = Just option
    | otherwise = Nothing  

menu :: IO Int
menu = do
        putStr "\n"
        print("[*--------------------------------------*]")
        print "| 1 - Uniao Ordenada                     |"
        print "| 2 - Lista de Somas                     |"
        print("[*--------------------------------------*]")
        putStr "\nDigite uma opcao >> "
        op <- getLine
        return (read op :: Int)

main :: IO()
main = do
        putStr "Entre com a Lista 1, no formato [1,2,3,4 ...] \n"
        putStr ">> "
        lista1 <- read <$> getLine :: IO [Int]
        putStr "Entre com a Lista 2, no formato [1,2,3,4 ...] \n"
        putStr ">> "
        lista2 <- read <$> getLine :: IO [Int]
        aplicacao lista1 lista2

aplicacao :: [Int] -> [Int] -> IO()
aplicacao lista1 lista2 = do
                        opcao <- menu
                        case checaEntrada opcao 1 2 of
                                Nothing -> do 
                                        print("Opcao invalida, tente novamente")
                                        aplicacao lista1 lista2
                                Just 1 -> do
                                        putStr "Uniao ordenada das Listas >> "
                                        print(uniaoOrdenada lista1 lista2)
                                        aplicacao lista1 lista2
                                Just 2 -> do
                                        putStr "Lista de Somas >> "
                                        print(listaDeSomas lista1 lista2)
                                        aplicacao lista1 lista2


l1 = [1,3,4,5]
l2 = [2,4,6,7]
[16,25,36,49]