module StringForInt where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Char

strForInt([], casa) = 0
strForInt(c:r, casa)
    | isDigit(c) = (digitToInt(c) * 10^casa) + strForInt(r, casa-1)
    | otherwise = strForInt(r, casa)

main = do
    hSetBuffering stdout NoBuffering
    digitos <- getLine
    print(strForInt(digitos,(length digitos)-1))