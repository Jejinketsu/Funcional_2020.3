module MMC where

miniDiv(n, divisor)
    | divisor <= 1 = miniDiv(n, divisor+1)
    | divisor > n = error "Divisor maior que n\n"
    | mod n divisor == 0 = divisor
    | otherwise = miniDiv(n, divisor+1)


verifica(x, cont)
    |x == 1 = cont
    |otherwise = verifica(x - (x/miniDiv(x)), cont+1)

fatoracao(x, y, z)
    | miniDiv(x, 2) == miniDiv(y, 2) && miniDiv(y, 2) == miniDiv(z, 2) = putStr miniDiv(x, 2) >> verifica()