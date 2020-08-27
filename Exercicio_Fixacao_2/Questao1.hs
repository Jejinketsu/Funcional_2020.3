module Multiplos where

test(num)
    | mod num 3 == 0 && mod num 2 /= 0 && mod num 5 /= 0 = 1
    | otherwise = 0

mult(i, j, cont)
    | i > j = mult(j, i, cont)
    | i == j = cont + test(i)
    | test(i) == 1 = mult(i+1, j, cont+1)
    | otherwise = mult(i+1, j, cont)

main = do
        putStr "Digite I: "
        i <- getLine
        putStr "Digite J: "
        j <- getLine
        print(mult(read i :: Int, read j :: Int, 0))