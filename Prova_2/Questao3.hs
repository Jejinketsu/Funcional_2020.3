module Tuplas where

-- (codigo, nome, tipo, quantidade, preco unt)

type Produto = (String, String, String, Int, Int)

teste :: [Produto]
teste = [("001","banana","alimento",12,1),("002","televisao","eletro",1,1200),("003","maca","alimento",5,1)]

-- a --
listaTipo :: [Produto] -> String -> [Produto]
listaTipo tuplas tipo = [(c,n,t,q,p) | (c,n,t,q,p) <- tuplas, t == tipo]

-- b --
qtdEstoqueTipo :: [Produto] -> String -> Int
qtdEstoqueTipo tuplas tipo = sum [q | (c,n,t,q,p) <- tuplas, t == tipo]

-- c --
calculaPreco :: Produto -> Int -> Int
calculaPreco (c,n,t,q,p) qtd
    | qtd <= q = qtd * p
    | otherwise = q * p

valorCompra :: [Produto] -> String -> Int -> Int
valorCompra tuplas codigo qtd
    | length buscaProduto > 0 = (calculaPreco ([(c,n,t,q,p) | (c,n,t,q,p) <- tuplas, c == codigo]!!0) qtd)
    | otherwise = 0 
        where
            buscaProduto = [(c,n,t,q,p) | (c,n,t,q,p) <- tuplas, c == codigo]