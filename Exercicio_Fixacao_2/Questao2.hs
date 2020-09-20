module SomaMultQuad where

test3(num)
    | mod num 3 == 0 = num
    | otherwise = 1 

test100(num)
    | mod 100 num == 0 = num
    | otherwise = 0 

multQuadSoma(i, j, somaM3, somaQ100)
    | i > j = multQuadSoma(j, i, somaM3, somaQ100)
    | i == 0 && j == 0 = 0
    | i == 0 = multQuadSoma(i+1, j, somaM3, somaQ100)
    | i == j && somaM3 == 1 && test3(i) == 1 = (somaQ100 + test100(i))^2
    | i == j = (somaM3 * test3(i)) + ((somaQ100 + test100(i))^2)
    | otherwise = multQuadSoma(i+1, j, (somaM3 * test3(i)), (somaQ100 + test100(i)))

main = do
        putStr "Digite I: "
        i <- getLine
        putStr "Digite J: "
        j <- getLine
        print(multQuadSoma(read i :: Int, read j :: Int, 1, 0))