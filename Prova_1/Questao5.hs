module Somatoria where

somatorio y x
    | x == y = y
    | x > y = y + somatorio (y+1) x

equacao x y m
    | x == m = x * (somatorio y x)
    | x < m = (x * (somatorio y x)) + equacao (x+1) 2 m