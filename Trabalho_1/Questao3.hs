module MDC where

mdc(x, y, divisor)
    | mod x divisor == 0 && mod y divisor == 0 = divisor
    | otherwise = mdc(x, y, divisor-1)
    
main :: IO()
main = do
        putStr "Digite o primeiro valor >> "
        x <- getLine
        putStr "Digite o segundo valor >> "
        y <- getLine
        putStr "Digite o segundo valor >> "
        z <- getLine
        print(mdc(mdc(read x, read y, read x), mdc(read y, read z, read y), read z))
      
