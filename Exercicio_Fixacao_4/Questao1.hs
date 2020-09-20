module Listas where

somaListas :: [Int] -> [Int] -> Int
somaListas l1 l2 = (sum l1) + (sum l2)

quadradoListas :: [Int] -> [Int] -> [Int]
quadradoListas l1 l2 = concat [(map (^2) l1), (map (^2) l2)]

maiorDasListas :: [Int] -> [Int] -> Int
maiorDasListas l1 l2 = maximum (concat [l1, l2])

mult3Listas :: [Int] -> [Int] -> [Int]
mult3Listas l1 l2 = concat [filter (\x -> mod x 3 == 0) l1, filter (\x -> mod x 3 == 0) l2]

prodNListas :: [Int] -> [Int] -> Int -> Int
prodNListas l1 l2 n = l1!!n * l2!!n

multQuadCuboListas :: [Int] -> [Int] -> [Int]
multQuadCuboListas l1 l2 = map (*5) (concat [map (^2) l1, map (^3) l2])
                            
checaEntrada :: Int -> Maybe Int
checaEntrada option
    | option >= 1 && option <= 6 = Just option
    | otherwise = Nothing

menu :: IO Int
menu = do
        print "Escolha uma das opcoes a seguir:"
        print "1. Somar a soma de duas listas;"
        print "2. Lista com os quadrado de duas listas;"
        print "3. Maior valor entre duas listas;"
        print "4. Apenas os multiplos de 3 das duas listas;"
        print "5. Produto entre o n-esimo valor das duas listas"
        print "6. Produto entre 5 e os quadrados e cubos das listas, respectivamente;"
        putStr "Digite sua escolha: "
        option <- getLine
        return (read option :: Int)

main :: IO ()
main = do
        putStr "Digite um lista no formato [1,2,..,n]: "
        l1 <- read <$> getLine :: IO [Int]
        putStr "Digite outra lista no mesmo formato: "
        l2 <- read <$> getLine :: IO [Int]
        option <- menu

        case checaEntrada option of
            Nothing -> do 
                        print "Entrada invalida"
                        print "Tente novamente"
                        main
            Just 1 -> print(somaListas l1 l2)
            Just 2 -> print(quadradoListas l1 l2)
            Just 3 -> print(maiorDasListas l1 l2)
            Just 4 -> print(mult3Listas l1 l2)
            Just 5 -> do
                    putStr "Digite a posicao n: "
                    n <- getLine
                    print(prodNListas l1 l2 (read n :: Int))
            Just 6 -> print(multQuadCuboListas l1 l2)

        putStr "Deseja continua? 1. sim; 2. nao: "
        option2 <- getLine

        if (read option2 :: Int) == 1 then main
        else print "Fim do programa."