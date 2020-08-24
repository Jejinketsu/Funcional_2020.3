module MMC where

mmcP(x, y, z, divisor)
    | divisor <= 1 = mmc(x, y, z, divisor+1)
    | divisor > x || divisor > y || divisor > z = error "Divisor maior que algum numero\n"
    | mod x divisor == 0 && mod y divisor == 0 && mod z divisor == 0 = 1
    | otherwise = 1 + mmc(x, y, z, divisor+1)

mmc(x, y, z, divisor)
    | divisor <= 1 = mmc(x, y, z, divisor+1)
    | divisor > x || divisor > y || divisor > z = error "Divisor maior que algum numero\n"
    | mod x divisor == 0 && mod y divisor == 0 && mod z divisor == 0 = divisor
    | otherwise = mmc(x, y, z, divisor+1)

main = do
        putStr "Digite X: "
        x <- getLine
        putStr "Digite Y: "
        y <- getLine
        putStr "Digite Z: "
        z <- getLine
        print(mmc(read x :: Int, read y :: Int, read z :: Int, 0))
        print(mmcP(read x :: Int, read y :: Int, read z :: Int, 0))