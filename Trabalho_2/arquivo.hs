module Main where

import Control.Exception
import System.IO
import System.IO.Error

import Prelude hiding (catch)

menu :: IO Int
menu = do
        putStrLn "1 - Cadastrar Pessoa"
        putStrLn "2 - Mostrar Todos"
        putStrLn "3 - Mostrar Dados uma Pessoa"
        putStrLn "4 - Excluir Pessoa"
        putStrLn "5 - Sair"
        putStrLn "\n Digite a opcao desejada: "
        op <- getLine
        return(read op :: Int)

caso(1) = cadastro_pessoa
caso(2) = mostrar_todos
caso(3) = mostrar_pessoa
caso(4) = do
           putStrLn "CPF: "
           cpf <- getLine
           excluir_pessoa cpf
caso(op) = putStr "\nOpcao Invalida!\n"

main = do
        opcao <- menu
        if opcao /= 5
         then do
               caso (opcao)
               main
         else putStr"\nObrigado por usar o sistema!"

le_arquivo :: IO String
le_arquivo =  catch testa_arquivo trataErro
                 where 
                  testa_arquivo = do
                                   {arq <-  openFile "pessoas.txt" ReadMode;
                                    conteudo <- (hGetContents arq);
                                    return (conteudo);
                                    }
                  trataErro erro = if isDoesNotExistError erro
                                    then do
                                          {arq <- openFile "pessoas.txt" WriteMode;
                                           hClose arq;
                                           le_arquivo;
                                           } 
                                    else ioError erro 


cadastro_pessoa :: IO()
cadastro_pessoa = do
                   putStrLn "CPF: "
                   cpf <- getLine
                   putStrLn "Nome: "
                   nm <- getLine
                   conteudo <- le_arquivo
                   pessoas <- (converteconteudo (conteudo))
                   case busca_cpf pessoas ("CPF: "++cpf) of
                    Nothing ->  do 
                                 putStrLn "Cadastro realizado com sucesso. "
                                 let pessoa = "CPF: "++cpf++"\n"++"Nome: "++nm++"\n"++"f\n"
                                 arq <- openFile "pessoas.txt" AppendMode
                                 hPutStrLn arq pessoa
                                 hClose arq
                    Just pessoa -> putStrLn ("A pessoa ja esta cadastrada: " ++ (foldl1 (\m n -> m ++ " " ++ n) pessoa))	

imprimeLista [[]] = putStr "\n"
imprimeLista ((a:b:[]):r)= do
                              putStrLn (a++ "  "++b)
                              imprimeLista r

mostrar_todos :: IO ( )
mostrar_todos = do
                 arq <- openFile "pessoas.txt" ReadMode
                 conteudo <- (hGetContents arq)
                 pessoas <- converteconteudo conteudo
                 imprimeLista (pessoas)
                 hClose arq



mostrar_pessoa :: IO()
mostrar_pessoa = do
                  putStrLn "CPF: "
                  cpf <- getLine
                  arq <- openFile "pessoas.txt" ReadMode
                  conteudo <- (hGetContents arq)
                  pessoas <- (converteconteudo (conteudo))
                  case busca_cpf pessoas ("CPF: "++cpf) of
                   Nothing -> do
                               hClose arq
                               putStrLn "CPF não cadastrado!!!!"
                   Just pessoa -> do
                                   hClose arq
                                   putStrLn (foldl1 (\m n -> m++" "++n) pessoa)


excluir_pessoa :: String -> IO()
excluir_pessoa cpf = do
	                  arq <- openFile "pessoas.txt" ReadMode
	                  conteudo <- (hGetContents arq)
	                  pessoas <- (converteconteudo (conteudo))
	                  let novo_conteudo = apaga_cpf pessoas ("CPF: "++cpf)
	                  aux_arq <- openFile "auxiliar.txt" WriteMode
	                  hPutStr aux_arq novo_conteudo
	                  hClose arq
	                  hClose aux_arq
	                  copiar "auxiliar.txt" "pessoas.txt"
            


converteconteudo :: String -> IO [[String]]

converteconteudo [] = return [[]]
converteconteudo conteudo = return (map (explodir '\n') ( explodir 'f' conteudo))

explodir :: Eq a => a -> [a] -> [[a]]
explodir a [ ] = [ ]
explodir a (c:r)
	| (takeWhile (/= a) (c:r)) == [ ] = explodir a r
	| c == a = (takeWhile (/= a) r) : explodir a (dropWhile (/= a) r)
	| otherwise = (takeWhile (/= a)(c:r)) : explodir a (dropWhile (/= a) (c:r))

info_cpf,info_nome :: [String] -> String
info_cpf (c:n:[]) = c
info_nome (c:n:[]) = n

copiar origem destino = do
			aux_arq <- openFile origem ReadMode
			conteudo <- (hGetContents aux_arq)
			arq <- openFile destino WriteMode
			hPutStr arq conteudo
			hClose aux_arq
			hClose arq


busca_cpf :: [[String]] -> String -> Maybe [String]
busca_cpf [[]] cpf = Nothing
busca_cpf (c:r) cpf
	| (info_cpf c) == cpf = Just c
	| otherwise = busca_cpf r cpf


apaga_cpf :: [[String]] -> String -> String
apaga_cpf [[]] cpf = "\n"
apaga_cpf (c:r) cpf 
	| cpf == (info_cpf c) = (apaga_cpf r cpf)
	| otherwise = (foldl1 (\m n->m++"\n"++n) c) ++ "f\n" ++ (apaga_cpf r cpf)

