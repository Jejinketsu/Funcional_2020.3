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


main = do
        putStr "Entre com a Lista 1, no formato [1,2,3,4 ...]: "
        lista1 <- read <$> getLine :: IO [Int]
        putStr "Entre com a Lista 2, no formato [1,2,3,4 ...]: "
        lista2 <- read <$> getLine :: IO [Int]
        opcao <- menu
        case checaEntrada opcao 1 2 of
            Nothing -> do 
                    print("Opcao invalida, tente novamente")
                    main
            Just 1 -> do
                    print(uniaoOrdenada lista1 lista2)
                    main
            Just 2 -> do
                    print(listaDeSomas lista1 lista2)
                    main

-- A = 
-- (A-B) = [1,8]
-- (B-A) = [7,9]