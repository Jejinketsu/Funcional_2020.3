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

verificaCondicao :: Bool -> Maybe Bool
verificaCondicao(operacao)
        | operacao = Just True
        | otherwise = Just False

--Funções auxiliares para as Notas
verificaMatricula :: [Aluno] -> IO String
verificaMatricula listaAluno = do
                imprimeTuplas(listaAluno, ["Matricula: ","Nome: ","Curso: ", "Periodo: "])
                putStr "\nDigite a Matricula do Aluno >> "
                matricula <- getLine
                case verificaCondicao (length ([mat | (mat, nome, curso, periodo) <- listaAluno, mat == matricula]) > 0) of
                        Just True -> return matricula
                        Just False -> do
                                print("Esse aluno nao existe. Tente outro.")
                                verificaMatricula listaAluno

verificaDisciplina :: [Disciplina] -> IO String
verificaDisciplina listaDisciplina = do
                imprimeTuplas(listaDisciplina, ["Codigo: ","Nome: ","Curso: ", "Periodo: "])
                putStr "\nDigite o codigo da Disciplina >> "
                cod_disciplina <- getLine
                case verificaCondicao (length ([disciplina | (disciplina, curso, nome, periodo) <- listaDisciplina, disciplina == cod_disciplina]) > 0) of
                        Just True -> return cod_disciplina
                        Just False -> do
                                putStr("Disciplina nao existe. Tente outra. \n")
                                verificaDisciplina listaDisciplina        
--End funções


validaCurso :: [Curso] -> IO String
validaCurso listaCurso = do
                imprimeCurso(listaCurso)
                putStr "\nDigite o codigo do Curso >> "
                codigo_curso <- getLine
                case verificaCondicao (length ([codigo | (codigo, nome, qtd_periodo) <- listaCurso, codigo == codigo_curso]) > 0) of
                        Just True -> return codigo_curso
                        Just False -> do
                                print("Esse curso nao existe. Tente outro.")
                                validaCurso listaCurso

validaPeriodo :: [Curso] -> String -> IO Int
validaPeriodo listaCurso cod_curso = do
        periodo <- getLine
        case verificaCondicao (length ([codigo | (codigo, nome, qtd_periodo) <- listaCurso, codigo == cod_curso, (read periodo :: Int) <= qtd_periodo, (read periodo :: Int) > 0]) > 0) of
                Just True -> return (read periodo :: Int)
                Just False -> do
                                print("Periodo invalido. Tente outro.")
                                validaPeriodo listaCurso cod_curso

lerNotas :: [Aluno] -> [Disciplina] -> IO Nota
lerNotas listaAlunos listaDisciplina = do
        putStr "Digite a matricula do aluno >> "
        matricula_aluno <- verificaMatricula listaAlunos
        putStr "Digite o codigo da disciplina >> "
        codigo_disciplina <- verificaDisciplina listaDisciplina
        putStr "Digite a nota 1 >> "
        nota_1 <- getLine
        putStr "Deseja cadastrar a SEGUNDA nota 1-sim | 2-nao >> "
        op <- getLine
        case verificaCondicao (read op == 1) of
                Just True -> do
                        putStr "Digite a nota 2 >> "
                        nota_2 <- getLine
                        putStr "Notas cadastradas com sucesso \n"
                        return (matricula_aluno, codigo_disciplina, read nota_1 :: Float, read nota_2 :: Float)
                Just False -> return (matricula_aluno, codigo_disciplina, read nota_1 :: Float, -1.0)

lerAluno :: [Curso] -> [Aluno] -> IO Aluno
lerAluno listaCurso listaAlunos = do
        putStr "Digite a matricula do aluno >> "
        matricula <- validaMatricula listaAlunos
        putStr "Digite o nome do aluno >> "
        nome_aluno <- getLine
        putStr "Digite o codigo do curso >> "
        codigo_curso <- validaCurso listaCurso
        putStr "Digite o periodo do aluno >> "
        periodo <- validaPeriodo listaCurso codigo_curso
        return (matricula, nome_aluno, codigo_curso, periodo)

validaMatricula :: [Aluno] -> IO String
validaMatricula listaAlunos = do
                matricula <- getLine
                case verificaCondicao (length ([mat | (mat, nome, curso, periodo) <- listaAlunos, mat == matricula]) > 0) of
                        Just True -> do
                                        print("Matricula ja existe. Tente outra.")
                                        putStr "Digite a matricula >> "
                                        validaMatricula listaAlunos
                        Just False -> return matricula
 
lerDisciplina :: [Disciplina] -> [Curso] -> IO Disciplina
lerDisciplina listaDisciplina listaCurso = do
        putStr "Digite o codigo da disciplina >> "
        codigo_disciplina <- validaDisciplina listaDisciplina
        putStr "Digite o código do curso >> "
        codigo_curso <- validaCurso listaCurso
        putStr "Digite o nome da disciplina >> "
        nome_disciplina <- getLine
        putStr "Digite o periodo da disciplina >> "
        periodo <- validaPeriodo listaCurso codigo_curso
        return (codigo_disciplina, codigo_curso, nome_disciplina, periodo)

validaDisciplina :: [Aluno] -> IO String
validaDisciplina listaDisciplina = do
                cod_disciplina <- getLine
                case verificaCondicao (length ([disciplina | (disciplina, curso, nome, periodo) <- listaDisciplina, disciplina == cod_disciplina]) > 0) of
                        Just True -> do
                                print("Disciplina ja existe. Tente outra.")
                                validaDisciplina listaDisciplina
                        Just False -> return cod_disciplina

checaEntrada :: Int -> Int -> Int -> Maybe Int
checaEntrada option min max
    | option >= min && option <= max = Just option
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
imprimeTuplas([], labels) = print("Nenhum caso.")
imprimeTuplas(c:r, labels)
    | (length r) /= 0 = do
                        imprime(c, labels, 0)
                        imprimeTuplas(r, labels)
    | otherwise = imprime(c, labels, 0)

insereElemento :: Ord a => ([a], [a]) -> [a]
insereElemento(c1:r1, []) = c1:r1
insereElemento(c1:r1, c2:r2)
    | c1 < c2 = concat[c1:r1, c2:r2]
    | c1 == c2 = c2:r2
    | otherwise = c2 : insereElemento(c1:r1, r2)

buscaAluno :: (String, [Aluno]) -> Bool
buscaAluno(matricula, []) = False
buscaAluno(matricula_busca, (matricula, nome, curso, periodo):r)
    | matricula_busca == matricula = True
    | otherwise = buscaAluno(matricula, r)

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