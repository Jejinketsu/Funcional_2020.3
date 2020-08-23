module Fatorial where

fatorial(n)
    |n == 0 = 1
    |otherwise = n * fatorial(n-1)
    
calc(n, x) = x * ((fatorial(n) * 5) / (fatorial(n-5)*2))

produtorio_CP(m, n)
    |m < 5 = 0
    |n == m = 1
    |otherwise = ((fatorial(n) * 5) / (fatorial(n-5)*2)) * produtorio_CP(m, n+1)

produtorio_SP(m, n, x)
    |m < 5 = 0
    |n == m = x
    |otherwise = produtorio_SP(m, n+1, calc(n, x))