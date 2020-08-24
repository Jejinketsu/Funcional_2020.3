module DivSimply where

divSubtractorS(x, y, cont)
    | y <= 0 = cont
    | x > y = divSubtractorS(y, x, 0)
    | otherwise = divSubtractorS(x, y-x, cont+1)

divSubtractor(x, y)
    | y <= 0 = 0
    | x > y = divSubtractor(y, x)
    | otherwise = 1 + divSubtractor(x, y-x)

main = do
        putStr "Digite X: "
        x <- getLine
        putStr "Digite Y: "
        y <- getLine
        print(divSubtractor(read x, read y))
        print(divSubtractorS(read x, read y, 0))