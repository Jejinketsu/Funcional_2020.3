module Main where

misterio i f result
    | i == f && odd i = result * (i^3)
    | i == f = result * (i^2)
    | i < f && odd i = misterio (i+1) f (result * (i^3))
    | otherwise = misterio (i+1) f (result * (i^2))

misterioP i f
    | i == f && odd i = (i^3)
    | i == f = (i^2)
    | i < f && odd i = (i^3) * (misterioP (i+1) f)
    | otherwise = (i^2) * (misterioP (i+1) f)

main = do
    print(misterio 20 45 1)
    print(misterioP 20 45)