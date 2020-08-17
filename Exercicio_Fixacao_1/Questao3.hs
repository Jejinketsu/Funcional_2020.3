module MaiorqMedia where

lerInt message = do
    putStrLn message
    a <- getLine
    return(read a :: Int)

media :: [Int] -> Int
media(list) = div (sum list) (length list)

comparar :: [Int] -> Int
comparar(list) = length [x | x <- list, x > media(list)]

main = do
    a <- lerInt "Digite A: "
    b <- lerInt "Digite B: "
    c <- lerInt "Digite C: "
    print(comparar([a,b,c]))