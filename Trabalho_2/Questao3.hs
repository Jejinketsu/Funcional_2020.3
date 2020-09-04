module Strings where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Char

palindromo([],[]) = True
palindromo(l1,[]) = False
palindromo([],l2) = False
palindromo(c1:r1, c2:r2)
    | toUpper(c1) == toUpper(c2) = True && palindromo(r1, r2)
    | otherwise = False

ehVogal(c)
    | c == 'a' || c == 'e' ||c == 'i' ||c == 'o' ||c == 'u' = True
    | otherwise = False

asterisco([]) = []
asterisco(c:r)
    | ehVogal(toLower(c)) = c:'*':asterisco(r)
    | otherwise = c:asterisco(r)

contaPalavras([]) = 1
contaPalavras(c:r)
    | c == ' ' = 1 + contaPalavras(r)
    | otherwise = contaPalavras(r)

main = do
    hSetBuffering stdout NoBuffering
    putStr "Digite uma palavra: "
    text <- getLine

    putStr "Palindromo: "
    print(palindromo(text, reverse text))
    putStr "Asteriscos: "
    print(asterisco(text))
    putStr "Palavras: "
    print(contaPalavras(text))
