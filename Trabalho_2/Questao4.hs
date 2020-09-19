module Aluno where

type Curso = (String, String, Int)
type Aluno = (String, String, String, Int)
type Disciplina = (String, String, String, Int)
type Nota = (String, String, Float, Float)

--Curso(Código, Nome, Quantidade de Períodos),
--Alunos(Matrícula, Nome, Curso, Período),
--Dissciplinas(Código da Disciplina, Código do Curso, Nome Disciplina, Período)
--Notas(Matricula, Código Disciplina, Nota1, Nota2)

lerCurso :: IO Curso
lerCurso = do
        putStr "Digite o codigo do curso >> "
        codigo <- getLine
        putStr "Digite o nome do curso >> "
        nome_curso <- getLine
        putStr "Digite quantidade de periodos do curso >> "
        qtd_periodos <- getLine
        return (codigo, nome_curso, read qtd_periodos :: Int)

lerNotas :: IO Nota
lerNotas = do
        putStr "Digite a matricula do aluno >> "
        matricula_aluno <- getLine
        putStr "Digite o codigo da disciplina >> "
        codigo_disciplina <- getLine
        putStr "Digite a nota 1 >> "
        nota_1 <- getLine
        putStr "Digite a nota 2 >> "
        nota_2 <- getLine
        return (matricula_aluno, codigo_disciplina, read nota_1 :: Float, read nota_2 :: Float)

lerAluno :: IO Aluno
lerAluno = do
        putStr "Digite a matricula do aluno >> "
        matricula <- getLine
        putStr "Digite o nome do aluno >> "
        nome_aluno <- getLine
        putStr "Digite o codigo do curso >> "
        codigo_curso <- getLine
        putStr "Digite o periodo do aluno >> "
        periodo <- getLine
        return (matricula, nome_aluno, codigo_curso, read periodo :: Int)

lerDisciplina :: IO Disciplina
lerDisciplina = do
        putStr "Digite o codigo da disciplina >> "
        codigo_disciplina <- getLine
        putStr "Digite o código do curso >> "
        codigo_curso <- getLine
        putStr "Digite o nome da disciplina >> "
        nome_disciplina <- getLine
        putStr "Digite o periodo da disciplina >> "
        periodo <- getLine
        return (codigo_disciplina, codigo_curso, nome_disciplina, read periodo :: Int)

checaEntrada :: Int -> Maybe Int
checaEntrada option
    | option >= 1 && option <= 8 = Just option
    | otherwise = Nothing        
           
imprimeCurso :: [Curso] -> IO ()
imprimeCurso([]) = putStr "\n"
imprimeCurso((codigo, nome_curso, qtd_periodos):r) = do
                                putStr "Codigo: " >> putStr(codigo)
                                putStr "   |   Nome: " >> putStr(nome_curso)
                                putStr "   |   Qtd. Periodos: " >> putStr(show qtd_periodos)
                                putStr "\n"
                                imprimeCurso(r)

imprime :: (Show a1, Show a2, Show a3, Show a4) => ((a1, a2, a3, a4), [String], Int) -> IO ()
imprime((a,b,c,d), labels, cont)
    | cont == 0 = do 
                    putStr(labels!!cont ++ show a ++ "  |  ")
                    imprime((a,b,c,d), labels, cont+1)
    | cont == 1 = do
                    putStr(labels!!cont ++ show b ++ "  |  ")
                    imprime((a,b,c,d), labels, cont+1)
    | cont == 2 = do 
                    putStr(labels!!cont ++ show c ++ "  |  ")
                    imprime((a,b,c,d), labels, cont+1)
    | cont == 3 = do 
                    putStr(labels!!cont ++ show d)
                    imprime((a,b,c,d), labels, cont+1)
    | otherwise = putStr "\n"

imprimeTuplas :: (Show a1, Show a2, Show a3, Show a4) => ([(a1, a2, a3, a4)], [String]) -> IO ()
imprimeTuplas(c:r, labels)
    | (length r) /= 0 = do
                        imprime(c, labels, 0)
                        imprimeTuplas(r, labels)
    | otherwise = imprime(c, labels, 0)

insereCurso :: ([Curso], [Curso]) -> [Curso]
insereCurso(c1:r1, []) = c1:r1
insereCurso(c1:r1, c2:r2)
    | c1 < c2 = concat[c1:r1, c2:r2]
    | c1 == c2 = c2:r2
    | otherwise = c2 : insereCurso(c1:r1, r2)

menuPrincipal :: IO Int
menuPrincipal = do
        putStr "\n"
        print("[*-----------------------------------*]")
        print "| 1 - Cadastrar Curso                 |"
        print "| 2 - Cadastrar Disciplina            |"
        print "| 3 - Cadastrar Aluno                 |"
        print "| 4 - Cadastrar Nota                  |"
        print "| 5 - Ver Cursos                      |"
        print "| 6 - Ver Alunos de um Curso          |"
        print "| 7 - Disciplinas de um Curso         |"
        print "| 8 - Notas de um Aluno               |"
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

aplicacao :: [Curso] -> [Aluno] -> [Disciplina] -> [Nota] -> IO ()
aplicacao listaCurso listaAluno listaDisciplina listaNota = do
                    option <- menuPrincipal
                    case checaEntrada option of
                        Nothing -> do 
                                    print "Entrada invalida"
                                    print "Tente novamente"
                                    aplicacao listaCurso listaAluno listaDisciplina listaNota
                        Just 1 -> do
                                    novoCurso <- lerCurso
                                    aplicacao (insereCurso([novoCurso], listaCurso)) listaAluno listaDisciplina listaNota
                        Just 2 -> do 
                                    novoAluno <- lerAluno
                                    aplicacao listaCurso listaAluno listaDisciplina listaNota
                        Just 3 -> do
                                    nonaDisciplina <- lerDisciplina
                                    aplicacao listaCurso listaAluno listaDisciplina listaNota
                        Just 4 -> do
                                    lerNotas <- lerNotas
                                    aplicacao listaCurso listaAluno listaDisciplina listaNota
                        Just 5 -> do
                                    imprimeCurso(listaCurso)
                                    aplicacao listaCurso listaAluno listaDisciplina listaNota
                        Just 6 -> do
                                    imprimeCurso(listaCurso)
                                    imprimeTuplas(listaAluno, ["Matricula: ","Nome: ","Curso: ", "Periodo: "])
                                    aplicacao listaCurso listaAluno listaDisciplina listaNota
                        Just 7 -> do
                                    imprimeTuplas(listaDisciplina, ["Matricula: ","Nome: ","Curso: ", "Periodo: "])
                                    aplicacao listaCurso listaAluno listaDisciplina listaNota
                        Just 8 -> do
                                    imprimeTuplas(listaNota, ["Matricula: ","Nome: ","Curso: ", "Periodo: "])
                                    aplicacao listaCurso listaAluno listaDisciplina listaNota
                        Just 9 -> print("Fim do programa.")

main :: IO ()
main = aplicacao [] [] [] [] 