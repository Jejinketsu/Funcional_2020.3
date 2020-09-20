module Tuplas where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Char

listaTeste = [("111","Juh",'F',18),("222","Medyna",'f',60),("333","GD",'M',18),("444","Glaubinho",'m',45)]  
-- [("111","Juh",'F',18),("222","Medyna",'f',60),("333","GD",'M',18),("444","Glaubinho",'m',45)]
-- [(07873102232, "Lucas", 'M', 10), (18873102202, "Mateus", 'M', 10), (29187723445, "Marcos", 'M', 30), (47800976523, "Emili", 'F', 50), (89076543212, "janaina", 'F', 60)]

contaMulheres([]) = 0
contaMulheres((cpf, nome, sexo, idade):r)
    | toLower(sexo) == 'f' = 1 + contaMulheres(r)
    | otherwise = contaMulheres(r)

conta40([]) = 0
conta40((cpf, nome, sexo, idade):r)
    | idade > 40 = 1 + conta40(r)
    | otherwise = conta40(r)

mulheres40(lista) = [nome | (cpf, nome, sexo, idade) <- lista, idade > 40, toLower(sexo) == 'f']

mediaIdadeHomens([], soma, qtd) = div soma qtd
mediaIdadeHomens((cpf, nome, sexo, idade):r, soma, qtd)
    | toLower(sexo) == 'm' = mediaIdadeHomens(r, soma+idade, qtd+1)
    | otherwise = mediaIdadeHomens(r, soma, qtd)

main = do
    hSetBuffering stdout NoBuffering
    putStr "Quantidade de Mulheres: "
    print(contaMulheres(listaTeste))
    putStr "Quantidade de maiores de 40 anos: "
    print(conta40(listaTeste))
    putStr "Mulheres com mais de quarenta: "
    print(mulheres40(listaTeste))
    putStr "Media de idade dos homens: "
    print(mediaIdadeHomens(listaTeste, 0, 0))