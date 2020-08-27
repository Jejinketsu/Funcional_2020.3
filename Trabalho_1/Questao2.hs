module MMC where

eh_primo(x, divisor)
    | x == 1 = True
    | divisor == 1 = True
    | mod x divisor == 0 = False
    | otherwise = eh_primo(x, divisor-1)
    
proxPrimo(n)
    | eh_primo(n+1, n) = n+1
    | otherwise = proxPrimo(n+1)

showNumber(x, y) = "O numero " ++ show(x) ++ " repetiu " ++ show(y) ++ " vez(es) | "

testDifZero(x, y, z, divisor)
    | (mod x divisor) /= 0 && (mod y divisor) /= 0 && (mod z divisor) /= 0 = True
    | otherwise = False

testDiv(num, divisor)
    | mod num divisor == 0 = div num divisor
    | otherwise = num

fat(x, y, z, divisor, cont, text)
    | (x == 1) && (y == 1) && (z == 1) = print(text ++ showNumber(divisor, cont))
    | testDifZero(x, y, z, divisor) && cont == 0 = fat(x, y, z, proxPrimo(divisor), 0, text)
    | testDifZero(x, y, z, divisor) = fat(x, y, z, proxPrimo(divisor), 0, text ++ showNumber(divisor, cont))
    | otherwise = fat(testDiv(x, divisor), testDiv(y, divisor), testDiv(z, divisor), divisor, cont+1, text)

main = do
        putStr "Digite o primeiro numero >> "
        x <- getLine
        putStr "Digite o segundo numero >> "
        y <- getLine
        putStr "Digite o terceiro numero >> "
        z <- getLine
        fat(read x, read y, read z, 2, 0, "")