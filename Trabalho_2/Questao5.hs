module Aluno where

import Control.Exception
import System.IO
import System.IO.Error

import Prelude hiding (catch)

type Curso = (String, String, Int)
type Aluno = (String, String, String, Int)
type Disciplina = (String, String, String, Int)
type Nota = (String, String, Float, Float)

lerCurso :: IO Curso
lerCurso = do
        putStr "Digite o codigo do curso >> "
        codigo <- getLine
        putStr "Digite o nome do curso >> "
        nome_curso <- getLine
        putStr "Digite quantidade de periodos do curso >> "
        qtd_periodos <- getLine
        return (codigo, nome_curso, read qtd_periodos :: Int)

le_arquivo :: String -> IO String
le_arquivo arquivo =  catch testa_arquivo trataErro
                    where 
                    testa_arquivo = do
                                    {arq <-  openFile arquivo ReadMode;
                                    conteudo <- (hGetContents arq);
                                    return (conteudo);
                                    }
                    trataErro erro = if isDoesNotExistError erro
                                    then do
                                            {arq <- openFile arquivo WriteMode;
                                            hClose arq;
                                            le_arquivo arquivo;
                                            } 
                                    else ioError erro
                                
converteconteudo :: String -> IO [[String]]
converteconteudo [] = return [[]]
converteconteudo conteudo = return (map (explodir '\n') ( explodir 'f' conteudo))

explodir :: Eq a => a -> [a] -> [[a]]
explodir a [ ] = [ ]
explodir a (c:r)
	| (takeWhile (/= a) (c:r)) == [ ] = explodir a r
	| c == a = (takeWhile (/= a) r) : explodir a (dropWhile (/= a) r)
	| otherwise = (takeWhile (/= a)(c:r)) : explodir a (dropWhile (/= a) (c:r))