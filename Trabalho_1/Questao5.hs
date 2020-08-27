module Cadeiras where

import System.Random (randomRIO)

mod3(x) = mod x 3

montaSaida(x,y,z) = "Ana: " ++ show(mod3(x)) ++ "  Beatriz: " ++ show(mod3(y)) ++ "  Carolina: " ++ show(mod3(z))

imprimeResultado(x, y)
    | mod3(x) == mod3(y) = montaSaida(x, x+1, x+2)
    | mod3(x+1) /= mod3(y) = montaSaida(x, y, x+1)
    | otherwise = montaSaida(x, y, x+2)
        
main = do 
        x <- randomRIO(1,100 :: Int)
        y <- randomRIO(1,100 :: Int)
        print(imprimeResultado(x,y))