module MMC where

mmcP(x, y, z, multiplo)
    | multiplo <= 1 = mmc(x, y, z, multiplo+1)
    | mod multiplo x == 0 && mod multiplo y == 0 && mod multiplo z == 0 = 1
    | otherwise = 1 + mmc(x, y, z, multiplo+1)

mmc(x, y, z, multiplo)
    | multiplo <= 1 = mmc(x, y, z, multiplo+1)
    | mod multiplo x == 0 && mod multiplo y == 0 && mod multiplo z == 0 = multiplo
    | otherwise = mmc(x, y, z, multiplo+1)

main = do
        putStr "Digite X: "
        x <- getLine
        putStr "Digite Y: "
        y <- getLine
        putStr "Digite Z: "
        z <- getLine
        print(mmc(read x :: Int, read y :: Int, read z :: Int, 0))
        print(mmcP(read x :: Int, read y :: Int, read z :: Int, 0))