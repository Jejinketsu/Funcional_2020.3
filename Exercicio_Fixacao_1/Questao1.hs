module AreaPoligono where

areaPoligono(forma, b, h)
 | forma == "quadrado" || forma == "retangulo" = b * h
 | forma == "triangulo" = (b * h) / 2
 | otherwise = error "Forma invalida."

main = do
    putStrLn "Digite a forma: "
    forma <- getLine
    putStrLn "Digite base: "
    b <- getLine
    putStrLn "Digite altura: "
    h <- getLine
    print(areaPoligono(forma, read b, read h))