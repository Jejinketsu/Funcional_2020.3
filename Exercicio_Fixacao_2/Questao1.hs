module Multiplos where

mult(i, j, cont, index)
    | index == j = cont
    | mod index 3 == 0 && mod index 2 /= 0 && mod index 5 /= 0 = mult(i, j, cont+1, index+1)
    | otherwise = mult(i, j, cont, index+1)

main = do
        putStr "Digite I: "
        i <- getLine
        putStr "Digite J: "
        j <- getLine
        print(mult(read i :: Int, read j :: Int, 0, 0))