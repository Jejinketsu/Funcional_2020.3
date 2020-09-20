module FormaTriangulo where

formaTriangulo(a,b,c)
 | a < bc && b < ac && c < ab = True
 | otherwise = False
    where
        ab = a + b 
        ac = a + c
        bc = b + c

main = do
    putStrLn "Digite a: "
    a <- getLine
    putStrLn "Digite b: "
    b <- getLine
    putStrLn "Digite c: "
    c <- getLine
    print(formaTriangulo(read a, read b, read c))