module DivSimply where

divSubtractorS(x, y, cont)
    | y <= 0 = -1
    | y == x = cont + 1
    | x > y = divSubtractorS(y, x, cont)
    | otherwise = divSubtractorS(x, y-x, cont+1)

divSubtractor(x, y)
    | y <= 0 = -1
    | x == y = 1
    | x > y = divSubtractor(y, x)
    | otherwise = 1 + divSubtractor(x, y-x)

main = do
        putStr "Digite X: "
        x <- getLine
        putStr "Digite Y: "
        y <- getLine
        print(divSubtractor(read x, read y))
        print(divSubtractorS(read x, read y, 0))