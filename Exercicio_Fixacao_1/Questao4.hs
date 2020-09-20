module Conversor where

setReference(und)
 | und == "Km" = 1000.0
 | und == "m" = 1.0
 | und == "cm" = 0.01
 | und == "mm" = 0.001
 | otherwise = error "Unidade invalida"

-- converte de und1 para und2
convert(und1, und2, value) = (setReference(und1) * value) / setReference(und2)

main = do
    putStrLn "Digite uma unidade: "
    und1 <- getLine
    putStrLn "Digite uma unidade: "
    und2 <- getLine
    putStrLn "Digite o valor: "
    value <- getLine
    print(convert(und1, und2, read value))