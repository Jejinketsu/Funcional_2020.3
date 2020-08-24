module SomaMultQuad where

multQuadSoma(i, j, somaM3, somaQ100)
    | i == j+1 = somaM3 + (somaQ100^2)
    | i == 0 = multQuadSoma(i+1, j, somaM3, somaQ100)
    | mod i 3 == 0 && mod 100 i == 0 = multQuadSoma(i+1, j, somaM3*i, somaQ100+i)
    | mod i 3 == 0 = multQuadSoma(i+1, j, somaM3*i,somaQ100)
    | mod 100 i == 0 = multQuadSoma(i+1, j, somaM3,somaQ100+i)
    | otherwise = multQuadSoma(i+1, j, somaM3, somaQ100)

main = do
        putStr "Digite I: "
        i <- getLine
        putStr "Digite J: "
        j <- getLine
        print(multQuadSoma(read i :: Int, read j :: Int, 1, 0))