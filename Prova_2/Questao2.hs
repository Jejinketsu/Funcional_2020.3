module Inteiros where

-- a --
somaCubo :: [Int] -> [Int] -> Int
somaCubo lista1 lista2 = sum [x^3 | x <- lista1 ++ lista2, x > 10]

-- b -- 
parMult3 :: [Int] -> [Int] -> [Int]
parMult3 lista1 lista2 = [x | x <- lista1, even x, mod x 3 == 0] ++ [x | x <- lista2, odd x, mod x 5 == 0]

-- c --
corteEmN :: [Int] -> [Int] -> Int -> [Int]
corteEmN lista1 lista2 n = (take n lista1) ++ (drop n lista2)

-- d -- 
produtorio :: [Int] -> [Int] -> Int
produtorio lista1 lista2 = product ([x^2 | x <- lista1, even x, x < 10] ++ [x^3 | x <- lista2, odd x, x < 5])