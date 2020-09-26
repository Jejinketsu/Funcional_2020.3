module Aluno where

import Control.Exception
import System.IO
import System.IO.Error
import Prelude hiding (catch)


le_arquivo :: String -> IO String
le_arquivo arquivo = catch testa_arquivo trataErro
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

--Inserção de Cursos
lerCurso :: IO()
lerCurso = do
        putStr "Digite o codigo do curso >> "
        codigo <- getLine
        putStr "Digite o nome do curso >> "
        nome_curso <- getLine
        putStr "Digite quantidade de periodos do curso >> "
        qtd_periodos <- getLine
        conteudo <- le_arquivo "curso.txt"
        cursos <- (converteconteudo (conteudo))
        case buscaCurso cursos codigo of
                Nothing ->  do 
                        putStrLn "Curso cadastrado com Sucesso. "
                        let info_curso = codigo ++ "\n" ++nome_curso ++ "\n" ++ qtd_periodos ++ "\n" ++ "f\n"
                        arq <- openFile "curso.txt" AppendMode
                        hPutStrLn arq info_curso
                        hClose arq
                Just info_curso -> putStrLn ("O curso ja esta cadastrado: " ++ (foldl1 (\m n -> m ++ " " ++ n) info_curso))
                

lerAluno :: IO()
lerAluno = do
        conteudo <- le_arquivo "aluno.txt"
        alunos <- (converteconteudo (conteudo))
        conteudo_curso <- le_arquivo "curso.txt"
        cursos <- (converteconteudo (conteudo_curso))
        matricula <- validaMatricula alunos
        putStr "Digite o nome do aluno >> "
        nome_aluno <- getLine
        codigo_curso <- validaCurso cursos
        --putStr "Digite o periodo do aluno >> "
        --periodo <- validaPeriodo listaCurso codigo_curso
        putStrLn "Curso cadastrado com Sucesso. "
        let info_aluno = matricula ++ "\n" ++nome_aluno ++ "\n" ++ codigo_curso ++ "\n" ++ "f\n"
        arq <- openFile "aluno.txt" AppendMode
        hPutStrLn arq info_aluno
        hClose arq

--Fim de inserção de Cursos

--Funções de Buscas
info_curso,info_aluno :: [String] -> String
info_curso (c:n:qtd:[]) = c
info_aluno (m:n:curso:p:[]) = m

buscaCurso :: [[String]] -> String -> Maybe [String]
buscaCurso [[]] codigo = Nothing
buscaCurso (c:r) codigo
        | (info_curso c) == codigo = Just c
        | otherwise = buscaCurso r codigo

buscaAluno :: [[String]] -> String -> Maybe [String]
buscaAluno [[]] mat = Nothing
buscaAluno (c:r) mat
        | (info_aluno c) == mat = Just c
        | otherwise = buscaAluno r mat

--Fim das funções de Buscas

--Funções de Validadações


validaMatricula :: [[String]] -> IO String
validaMatricula alunos = do
                putStr "Digite a matricula do aluno >> "
                matricula <- getLine
                case verificaCondicao(length(buscaAluno alunos matricula)>0) of
                        Just True -> do
                                        print("Matricula ja existe. Tente outra.")
                                        validaMatricula alunos
                        Just False -> return matricula

validaCurso :: [[String]] -> IO String
validaCurso cursos = do
                imprimeCurso(cursos)
                putStr "Digite o codigo do Curso >> "
                codigo <- getLine
                case verificaCondicao (length(buscaCurso cursos codigo)>0) of
                        Just True -> return codigo
                        Just False -> do
                                putStr "Curso nao existe. tente novamente\n"
                                validaCurso cursos


verificaCondicao :: Bool -> Maybe Bool
verificaCondicao(operacao)
        | operacao = Just True
        | otherwise = Just False
--Funções de Impressão

imprimeCurso :: [[String]] -> IO ()
imprimeCurso([[]]) = putStr "\n"
imprimeCurso((c:n:q:[]):r) = do
                                putStr "Codigo: " >> putStr(c)
                                putStr "   |   Nome: " >> putStr(n)
                                putStr "   |   Qtd. Periodos: " >> putStr(show q)
                                putStr "\n"
                                imprimeCurso(r)

--Fim de funções de Impressão
{-
menuPrincipal :: IO Int
menuPrincipal = do
        putStr "\n"
        print("[*-----------------------------------*]")
        print "| 1 - Cadastrar Curso                 |"
        print "| 2 - Cadastrar Aluno                 |"
        print "| 3 - Cadastrar Disciplina            |"
        print "| 4 - Cadastrar Nota                  |"
        print "| 5 - Ver Cursos                      |"
        print "| 6 - Ver Alunos de um Curso          |"
        print "| 7 - Ver Disciplinas de um Curso     |"
        print "| 8 - Ver Notas de um Aluno           |"
        print "| 9 - Finalizar                       |"
        print("[*-----------------------------------*]")
        putStr "\nDigite uma opcao >> "
        op <- getLine
        return (read op :: Int)

menuAluno :: IO Int
menuAluno = do
        putStr "\n"
        print("[*-----------------------------------------*]")
        print "| 1 - Ver todos os alunos do Curso          |"
        print "| 2 - Ver Alunos de um periodo do Curso     |"
        print "| 3 - Voltar                                |"
        print("[*-----------------------------------------*]")
        putStr "\nDigite uma opcao >> "
        op <- getLine
        return (read op :: Int)

menuDisciplina :: IO Int
menuDisciplina = do
        putStr "\n"
        print("[*---------------------------------------------*]")
        print "| 1 - Ver todas as disciplinas do Curso         |"
        print "| 2 - Ver disciplinas de um periodo do Curso    |"
        print "| 3 - Voltar                                    |"
        print("[*---------------------------------------------*]")
        putStr "\nDigite uma opcao >> "
        op <- getLine
        return (read op :: Int)

aplicacaoVerAluno :: [Curso] -> [Aluno] -> IO()
aplicacaoVerAluno listaCurso listaAluno = do
        option <- menuAluno
        case checaEntrada option 1 3 of
                Nothing -> do 
                        print "Entrada invalida"
                        print "Tente novamente"
                        aplicacaoVerAluno listaCurso listaAluno
                Just 1 -> do
                        codigo <- validaCurso listaCurso
                        imprimeTuplas([(mat, nome_aluno, curso, periodo) | (mat, nome_aluno, curso, periodo) <- listaAluno, (cod_curso, nome_curso, qtd_periodo) <- listaCurso, curso == cod_curso, codigo == cod_curso], ["Matricula: ","Nome: ","Curso: ", "Periodo: "])
                        aplicacaoVerAluno listaCurso listaAluno
                Just 2 -> do
                        codigo <- validaCurso listaCurso
                        periodo <- validaPeriodo listaCurso codigo
                        imprimeTuplas([(mat, nome_aluno, curso, periodo_aluno) | (mat, nome_aluno, curso, periodo_aluno) <- listaAluno, (cod_curso, nome_curso, qtd_periodo) <- listaCurso, curso == cod_curso, periodo_aluno == periodo], ["Matricula: ","Nome: ","Curso: ", "Periodo: "])
                        aplicacaoVerAluno listaCurso listaAluno
                Just 3 -> print("Voltando...")

aplicacaoVerDisciplina :: [Curso] -> [Disciplina] -> IO()
aplicacaoVerDisciplina listaCurso listaDisciplina = do
        option <- menuDisciplina
        case checaEntrada option 1 3 of
                Nothing -> do 
                        print "Entrada invalida"
                        print "Tente novamente"
                        aplicacaoVerDisciplina listaCurso listaDisciplina
                Just 1 -> do
                        codigo <- validaCurso listaCurso
                        imprimeTuplas([(cod_disc, cod_disc_curso, nome, periodo) | (cod_disc, cod_disc_curso, nome, periodo) <- listaDisciplina, (cod_curso, nome_curso, qtd_periodo) <- listaCurso, cod_disc_curso == cod_curso, codigo == cod_curso], ["Codigo do Curso: ","Codigo da Disciplina: ","Nome: ", "Periodo: "])
                        aplicacaoVerDisciplina listaCurso listaDisciplina
                Just 2 -> do
                        codigo <- validaCurso listaCurso
                        periodo_lido <- validaPeriodo listaCurso codigo
                        imprimeTuplas([(cod_disc, cod_disc_curso, nome, periodo) | (cod_disc, cod_disc_curso, nome, periodo) <- listaDisciplina, (cod_curso, nome_curso, qtd_periodo) <- listaCurso, cod_disc_curso == cod_curso, codigo == cod_curso, periodo == periodo_lido], ["Codigo do Curso: ","Codigo da Disciplina: ","Nome: ", "Periodo: "])
                        aplicacaoVerDisciplina listaCurso listaDisciplina
                Just 3 -> print("Voltando...")
                
aplicacao :: [Curso] -> [Aluno] -> [Disciplina] -> [Nota] -> IO ()
aplicacao listaCurso listaAluno listaDisciplina listaNota = do
                        option <- menuPrincipal
                        case checaEntrada option 1 9 of
                        Nothing -> do 
                                        print "Entrada invalida"
                                        print "Tente novamente"
                                        aplicacao listaCurso listaAluno listaDisciplina listaNota
                        Just 1 -> do
                                        novoCurso <- lerCurso
                                        aplicacao (insereElemento([novoCurso], listaCurso)) listaAluno listaDisciplina listaNota
                        Just 2 -> do 
                                        case verificaCondicao ((length listaCurso) == 0) of
                                        Just True -> do
                                                putStr "Nao eh possivel cadastrar aluno, pois nao ha nenhum curso cadastrado. Por favor, cadastre um Curso!"
                                                aplicacao listaCurso listaAluno listaDisciplina listaNota
                                        Just False -> do 
                                                novoAluno <- lerAluno listaCurso listaAluno
                                                aplicacao listaCurso (insereElemento([novoAluno], listaAluno)) listaDisciplina listaNota
                        Just 3 -> do
                                case verificaCondicao ((length listaCurso) == 0) of
                                        Just True -> do
                                                putStr "Nao eh possivel cadastrar disciplina, pois nao ha nenhum curso cadastrado. Por favor, cadastre um Curso!"
                                                aplicacao listaCurso listaAluno listaDisciplina listaNota
                                        Just False -> do
                                                novaDisciplina <- lerDisciplina listaDisciplina listaCurso
                                                aplicacao listaCurso listaAluno (insereElemento([novaDisciplina], listaDisciplina)) listaNota
                        Just 4 -> do
                                case verificaCondicao ((length listaAluno) == 0 || (length listaDisciplina) == 0) of
                                        Just True -> do
                                                putStr "Nao eh possivel cadastrar Nota, pois nao ha nenhum Aluno ou Disciplina cadastrada!"
                                                aplicacao listaCurso listaAluno listaDisciplina listaNota
                                        Just False -> do                                      
                                                notas <- lerNotas listaAluno listaDisciplina
                                                aplicacao listaCurso listaAluno listaDisciplina (insereElemento([notas], listaNota))
                        Just 5 -> do
                                        imprimeCurso(listaCurso)
                                        aplicacao listaCurso listaAluno listaDisciplina listaNota
                        Just 6 -> do
                                        aplicacaoVerAluno listaCurso listaAluno                                    
                                        aplicacao listaCurso listaAluno listaDisciplina listaNota
                        Just 7 -> do
                                        aplicacaoVerDisciplina listaCurso listaDisciplina
                                        aplicacao listaCurso listaAluno listaDisciplina listaNota
                        Just 8 -> do
                                        aluno <- verificaMatricula listaAluno
                                        imprimeTuplas([(mat, codigo_disciplina, nota1, nota2) | (mat, codigo_disciplina, nota1, nota2) <- listaNota, aluno==mat], ["Matricula: ","Codigo da Disciplina:", "Nota 1: ", "Nota 2: "])
                                        aplicacao listaCurso listaAluno listaDisciplina listaNota
                        Just 9 -> print("Fim do programa.")

main :: IO ()
main = aplicacao [] [] [] [] 

-}