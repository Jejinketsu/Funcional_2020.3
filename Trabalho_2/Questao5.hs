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
converteconteudo conteudo = return (map (explodir '\n') ( explodir '#' conteudo))

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
                        let info_curso = codigo ++ "\n" ++nome_curso ++ "\n" ++ qtd_periodos ++ "\n" ++ "#\n"
                        arq <- openFile "curso.txt" AppendMode
                        hPutStrLn arq info_curso
                        hClose arq
                Just info_curso -> putStrLn ("O curso ja esta cadastrado: " ++ (foldl1 (\m n -> m ++ " " ++ n) info_curso))
       
        
conteudoConvertido :: String -> IO [[String]]
conteudoConvertido arquivo = do
        conteudo_arquivo <- le_arquivo arquivo
        conteudo_lista <- (converteconteudo (conteudo_arquivo))
        return conteudo_lista


lerAluno :: IO()
lerAluno = do
        alunos <- conteudoConvertido "aluno.txt"
        cursos <- conteudoConvertido "curso.txt"
        matricula <- validaMatricula alunos
        putStr "Digite o nome do aluno >> "
        nome_aluno <- getLine
        codigo_curso <- validaCurso cursos
        periodo <- validaPeriodo(cursos, codigo_curso)
        putStrLn "Curso cadastrado com Sucesso. "
        let info_aluno = matricula ++ "\n" ++ nome_aluno ++ "\n" ++ codigo_curso ++ "\n" ++ show(periodo) ++ "\n" ++ "#\n"
        arq <- openFile "aluno.txt" AppendMode
        hPutStrLn arq info_aluno
        hClose arq

lerDisciplina :: IO()
lerDisciplina = do
        disciplinas <- conteudoConvertido "disciplina.txt"
        cursos <- conteudoConvertido "curso.txt"
        codigo_disciplina <- validaDisciplina disciplinas
        putStr "Digite o código do curso >> "
        codigo_curso <- validaCurso cursos
        putStr "Digite o nome da disciplina >> "
        nome_disciplina <- getLine
        periodo <- validaPeriodo(cursos, codigo_curso)
        let info_disciplina = codigo_disciplina ++ "\n" ++ codigo_curso ++ "\n" ++ nome_disciplina ++ "\n" ++ show(periodo) ++ "\n" ++ "#\n"
        arq <- openFile "disciplinas.txt" AppendMode
        hPutStrLn arq info_disciplina
        hClose arq


lerNotas :: IO()
lerNotas = do
        matricula_aluno <- verificaMatricula
        codigo_disciplina <- verificaDisciplina
        putStr "Digite a nota 1 >> "
        nota_1 <- getLine
        putStr "Deseja cadastrar a SEGUNDA nota 1-sim | 2-nao >> "
        op <- getLine
        case verificaCondicao (read op == 1) of
                Just True -> do
                        putStr "Digite a nota 2 >> "
                        nota_2 <- getLine
                        putStr "Notas cadastradas com sucesso \n"
                        let info_nota = matricula_aluno ++ "\n" ++ codigo_disciplina ++ "\n" ++ show(nota_1) ++ "\n" ++ show(nota_2) ++ "\n" ++ "#\n"
                        arq <- openFile "notas.txt" AppendMode
                        hPutStrLn arq info_nota
                        hClose arq                        
                Just False -> do
                        let info_nota = matricula_aluno ++ "\n" ++ codigo_disciplina ++ "\n" ++ show(nota_1) ++ "\n" ++ "-1" ++ "\n" ++ "#\n"
                        arq <- openFile "notas.txt" AppendMode
                        hPutStrLn arq info_nota
                        hClose arq 
--Fim de inserção de Cursos




--Funções auxiliares para as Notas
verificaMatricula :: IO String
verificaMatricula = do
                alunos <- conteudoConvertido "aluno.txt"
                imprimeAluno(alunos)
                putStr "\nDigite a Matricula do Aluno >> "
                matricula <- getLine
                case verificaCondicao (length ([mat:nome:curso:periodo:[] | mat:nome:curso:periodo:[] <- alunos, mat == matricula]) > 0) of
                        Just True -> return matricula
                        Just False -> do
                                print("Esse aluno nao existe. Tente outro.")
                                verificaMatricula

verificaDisciplina :: IO String
verificaDisciplina  = do
                disciplinas <- conteudoConvertido "disciplinas.txt"
                imprimeDisciplina(disciplinas)
                putStr "\nDigite o codigo da Disciplina >> "
                cod_disciplina <- getLine
                case verificaCondicao (length ([disciplina:curso:nome:periodo:[] | disciplina:curso:nome:periodo:[] <- disciplinas, disciplina == cod_disciplina]) > 0) of
                        Just True -> return cod_disciplina
                        Just False -> do
                                putStr("Disciplina nao existe. Tente outra. \n")
                                verificaDisciplina    
--End funções  


--Funções de Buscas
info_curso,info_aluno :: [String] -> String
info_curso (c:n:qtd:[]) = c
info_aluno (m:n:curso:periodo:[]) = m

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
                case verificaCondicao((length(buscaAluno alunos matricula))>0) of
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

                        
validaPeriodo :: ([[String]], String) -> IO Int
validaPeriodo (curso, cod_curso) = do
        putStr "Digite o periodo >> "
        periodo <- getLine
        case verificaCondicao (length ([codigo | codigo:nome:qtd_periodo:[] <- curso, codigo == cod_curso, (read periodo :: Int) <= (read qtd_periodo :: Int), (read periodo :: Int) > 0]) > 0) of
                Just True -> return (read periodo :: Int)
                Just False -> do
                                print("Periodo invalido. Tente outro.")
                                validaPeriodo(curso, cod_curso)

validaDisciplina :: [[String]] -> IO String
validaDisciplina disciplinas = do
                putStr "Digite o codigo da disciplina >> "
                cod_disciplina <- getLine
                case verificaCondicao (length ([disciplina | disciplina:curso:nome:periodo:[] <- disciplinas, disciplina == cod_disciplina]) > 0) of
                        Just True -> do
                                print("Disciplina ja existe. Tente outra.")
                                validaDisciplina disciplinas
                        Just False -> return cod_disciplina
                        
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

imprimeAluno :: [[String]] -> IO()
imprimeAluno([[]]) = putStr "\n"
imprimeAluno((m:n:c:p:[]):r) = do
                                putStr "Matricula: " >> putStr(m)
                                putStr "   |   Nome: " >> putStr(n)
                                putStr "   |   Curso: " >> putStr(c)
                                putStr "   |   Periodo: " >> putStr(show p)
                                putStr "\n"
                                imprimeAluno(r)

imprimeDisciplina :: [[String]] -> IO()
imprimeDisciplina([[]]) = putStr "\n"
imprimeDisciplina((cd:cc:n:p:[]):r) = do
                                putStr "Codigo da Disciplina: " >> putStr(cd)
                                putStr "   |   Codigo do curso: " >> putStr(cc)
                                putStr "   |   Nome: " >> putStr(n)
                                putStr "   |   Periodo: " >> putStr(show p)
                                putStr "\n"
                                imprimeDisciplina(r)
--Fim de funções de Impressão


checaEntrada :: Int -> Int -> Int -> Maybe Int
checaEntrada option min max
        | option >= min && option <= max = Just option
        | otherwise = Nothing 

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
               
        
aplicacaoVerAluno :: IO()
aplicacaoVerAluno = do
        option <- menuAluno
        case checaEntrada option 1 3 of
                Nothing -> do 
                        print "Entrada invalida"
                        print "Tente novamente"
                        aplicacaoVerAluno
                Just 1 -> do
                        cursos <- conteudoConvertido "curso.txt"
                        alunos <- conteudoConvertido "aluno.txt"
                        codigo <- validaCurso cursos
                        print([mat:nome_aluno:curso:periodo:[] | mat:nome_aluno:curso:periodo:[] <- alunos, cod_curso:nome_curso:qtd_periodo:[] <- cursos, curso == cod_curso, codigo == cod_curso])
                        aplicacaoVerAluno
                Just 2 -> do
                        cursos <- conteudoConvertido "curso.txt"
                        alunos <- conteudoConvertido "aluno.txt"
                        codigo <- validaCurso cursos
                        periodo <- validaPeriodo(cursos, codigo)
                        print([mat:nome_aluno:curso:periodo_aluno:[] | mat:nome_aluno:curso:periodo_aluno:[] <- alunos, cod_curso:nome_curso:qtd_periodo:[] <- cursos, curso == cod_curso, codigo == cod_curso, (read periodo_aluno :: Int )== periodo])
                        aplicacaoVerAluno
                Just 3 -> print("Voltando...")

aplicacaoVerDisciplina :: IO()
aplicacaoVerDisciplina = do
        option <- menuDisciplina
        case checaEntrada option 1 3 of
                Nothing -> do 
                        print "Entrada invalida"
                        print "Tente novamente"
                        aplicacaoVerDisciplina
                Just 1 -> do
                        cursos <- conteudoConvertido "curso.txt"
                        disciplinas <- conteudoConvertido "disciplinas.txt"
                        codigo <- validaCurso cursos
                        print([cod_disc:cod_disc_curso:nome:periodo:[] | cod_disc:cod_disc_curso:nome:periodo:[] <- disciplinas, cod_curso:nome_curso:qtd_periodo:[] <- cursos, cod_disc_curso == cod_curso, codigo == cod_curso])
                        aplicacaoVerDisciplina
                Just 2 -> do
                        cursos <- conteudoConvertido "curso.txt"
                        disciplinas <- conteudoConvertido "disciplinas.txt"
                        codigo <- validaCurso cursos
                        periodo_lido <- validaPeriodo(cursos, codigo)
                        print([cod_disc:cod_disc_curso:nome:periodo:[] | cod_disc:cod_disc_curso:nome:periodo:[] <- disciplinas, cod_curso:nome_curso:qtd_periodo:[] <- cursos, cod_disc_curso == cod_curso, codigo == cod_curso, (read periodo :: Int) == periodo_lido])
                        aplicacaoVerDisciplina
                Just 3 -> print("Voltando...")

aplicacao :: IO ()
aplicacao = do
                option <- menuPrincipal
                case checaEntrada option 1 9 of
                        Nothing -> do 
                                print "Entrada invalida"
                                print "Tente novamente"
                                aplicacao
                        Just 1 -> do
                                lerCurso
                                aplicacao
                        Just 2 -> do 
                                cursos <- conteudoConvertido "curso.txt"
                                case verificaCondicao((length cursos) == 1) of
                                        Just True -> do
                                                putStr "Nao eh possivel cadastrar aluno, pois nao ha nenhum curso cadastrado. Por favor, cadastre um Curso!"
                                                aplicacao
                                        Just False -> do 
                                                lerAluno
                                                aplicacao
                        Just 3 -> do
                                cursos <- conteudoConvertido "curso.txt"
                                case verificaCondicao((length cursos) == 1) of
                                        Just True -> do
                                                putStr "Nao eh possivel cadastrar disciplina, pois nao ha nenhum curso cadastrado. Por favor, cadastre um Curso!"
                                                aplicacao
                                        Just False -> do
                                                lerDisciplina
                                                aplicacao     
                        Just 4 -> do
                                alunos <- conteudoConvertido "aluno.txt"
                                disciplinas <- conteudoConvertido "disciplinas.txt"
                                print(length alunos)
                                print(length disciplinas)
                                case verificaCondicao ((length alunos) == 1 || (length disciplinas) == 1) of
                                        Just True -> do
                                                putStr "Nao eh possivel cadastrar Nota, pois nao ha nenhum Aluno ou Disciplina cadastrada!"
                                                aplicacao
                                        Just False -> do                                      
                                                lerNotas
                                                aplicacao
                        Just 5 -> do
                                cursos <- conteudoConvertido "curso.txt"
                                imprimeCurso(cursos)
                                aplicacao
                               
                        Just 6 -> do
                                aplicacaoVerAluno
                                aplicacao
                        Just 7 -> do
                                aplicacaoVerDisciplina
                                aplicacao
                        Just 8 -> do
                                alunos <- conteudoConvertido "aluno.txt"
                                notas <- conteudoConvertido "notas.txt"
                                aluno <- verificaMatricula
                                print([mat:codigo_disciplina:nota1:nota2:[] | mat:codigo_disciplina:nota1:nota2:[] <- notas, aluno==mat])
                                aplicacao
                        Just 9 -> print("Fim do programa.")
