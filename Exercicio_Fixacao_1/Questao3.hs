module MaiorqMedia where

media(a, b, c) = (a + b + c)/3

comparar(a, b, c)
    | a > med && b > med && c > med = 3
    | (a > med && b > med) || (b > med && c > med) || (a > med && c > med) = 2
    | a > med || b > med || c > med = 1
    | otherwise = 0
        where
            med = media(a, b, c)

main = do
    putStr "Digite a: "
    a <- getLine
    putStr "Digite b: "
    b <- getLine
    putStr "Digite c: "
    c <- getLine
    print(comparar(read a, read b, read c))