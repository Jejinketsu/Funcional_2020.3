module Calculo where

solver(a, b)
 | a > b = a + ((a/b) ** 2)
 | a < b = b * (b - a)
 | a == b = (a ** 3) + (a + b) ** 2