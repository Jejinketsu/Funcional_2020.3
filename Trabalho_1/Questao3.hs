module MDC where

mdc(x, y, divisor)
    | mod x divisor == 0 && mod y divisor == 0 = divisor
    | otherwise = mdc(x, y, divisor-1)

maxNumber(x, y)
    |x >= y = x
    |otherwise = y

minNumber(x, y)
    |x <= y = x
    |otherwise = y
    
    
main :: IO()
main = do
        putStr "Digite o primeiro valor >> "
        x <- getLine
        putStr "Digite o segundo valor >> "
        y <- getLine
        putStr "Digite o segundo valor >> "
        z <- getLine
        print(minNumber(mdc(read x, read y, maxNumber(read x, read y)), mdc(read x, read z, maxNumber(read x, read z))))
      