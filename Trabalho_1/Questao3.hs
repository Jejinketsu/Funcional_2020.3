module MDC where

mdc(x, y, divisor)
    | mod x divisor == 0 && mod y divisor == 0 = divisor
    | otherwise = mdc(x, y, divisor-1)

maxNumber(x, y)
    |x >= y = x
    |otherwise = y

resultadoMDC(x, y, z)
    | n1 >= n2 = mdc(n1, n2, n1)
    | otherwise = mdc(n1, n2, n2)
        where 
            n1 = mdc(x, y, maxNumber(x, y)) 
            n2 = mdc(x, z, maxNumber(x, z)) 

main :: IO()
main = do
        putStr "Digite o primeiro valor >> "
        x <- getLine
        putStr "Digite o segundo valor >> "
        y <- getLine
        putStr "Digite o segundo valor >> "
        z <- getLine
        --putStr "O mdc entre " ++ show(x) ++ show(y) ++ show(z) ++ "eh: "
        print(resultadoMDC(read x, read y, read z))
        