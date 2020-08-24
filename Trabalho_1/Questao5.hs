module Replacer where

slice(i, j, text) = drop i (take j text)

getInt :: [Char] -> Int
getInt(x) = read x :: Int

-- pega o maior número
maxDig(numText, i, j, digit)
    | j > length numText = i
    | digit < getInt(slice(i, j, numText)) = maxDig(numText, i+1, j+1, getInt(slice(i, j, numText)))
    | otherwise = maxDig(numText, i+1, j+1, digit)

-- 123456789 => 923456781 => 9,234567,8,1"" => 9,8,34567,12
switchNums(i, j, offset, text) = slice(0, offset, text) ++ slice(i, j, text) ++ slice(offset, i, text) ++ slice(j, length text, text)

replacer(n, t, offset)
    | t == 0 = n
    | otherwise = replacer(switchNums(i-1, i, offset, n), t-1, offset + 1)
        where
            i = maxDig(n, 0, 1, 0)
{-
main = do
    putStr "Digite N: "
    n <- getLine
    putStr "Digite T: "
    t <- getLine
    print(replacer(n, t, 0))
-}