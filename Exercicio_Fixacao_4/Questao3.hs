module Pessoas where

import Data.Char ( isDigit, digitToInt )

type CPF = String
type Nome = String
type Data = (Int, Int, Int)
type Sexo = Char
type Pessoa = (CPF, Nome, Data, Sexo)
type ListaPessoa = [Pessoa]

produtorioCPF :: CPF -> [Int] -> Int
produtorioCPF cpf mults = sum [digitToInt(x) * y | (x,y) <- zip cpf mults, isDigit(x)]

dig1valido :: CPF -> Bool
dig1valido cpf
    | mod ((produtorioCPF cpf (reverse [2..10])) * 10) 11 == digitToInt(cpf!!((length cpf) - 2)) = True
    | otherwise = False

dig2valido :: CPF -> Bool
dig2valido cpf
    | mod ((produtorioCPF cpf (reverse [2..11])) * 10) 11 == digitToInt(cpf!!((length cpf) - 1)) = True
    | otherwise = False

checaCPF :: CPF -> Bool
checaCPF cpf
    | dig1valido cpf && dig2valido cpf = True
    | otherwise = False

ehBissexto :: Int -> Bool
ehBissexto ano
    | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
    | (mod ano 4 /= 0) && (mod ano 400 == 0) = True
    | otherwise = False

checaDia :: Data -> Bool
checaDia (dia, mes, ano)
    | dia < 1 || dia > 31 = False
    | dia > 29 && mes == 2 = False
    | dia == 29 && mes == 2 && not (ehBissexto ano) = False
    | dia > 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11) = False
    | otherwise = True

checaMes :: Int -> Bool
checaMes mes
    | mes > 12 || mes < 1 = False
    | otherwise = True

checaAno :: Int -> Bool
checaAno ano
    | ano > 2020 || ano < 0 = False
    | otherwise = True

checaData :: Data -> Bool
checaData (dia, mes, ano)
    | (checaAno ano) && (checaMes mes) && (checaDia (dia, mes, ano)) = True
    | otherwise = False

recebeCPF :: IO CPF
recebeCPF = do
            putStr "Digite seu cpf apenas com numeros: "
            cpf <- getLine
            if not (checaCPF (read cpf :: CPF)) then do 
                                                        print("CPF invalido.")
                                                        recebeCPF
            else return (read cpf :: CPF)

recebeData :: IO Data
recebeData = do
            putStr "Digite uma data de nascimento valida no formato (dia, mes, ano): "
            dataNasc <- getLine
            if not (checaData (read dataNasc :: Data)) then do 
                                                                print("Data invalida.")
                                                                recebeData
            else return (read dataNasc :: Data)

recebeNome :: IO Nome
recebeNome = do
                putStr "Digite o nome: "
                nome <- getLine
                return (read nome :: Nome)

recebeSexo :: IO Sexo
recebeSexo = do
                putStr "Digite seu sexo (M/F): "
                sexo <- getLine
                return (read sexo :: Sexo)

cadastraPessoa :: IO Pessoa
cadastraPessoa = do
                    nome <- recebeNome
                    sexo <- recebeSexo
                    dataNasc <- recebeData
                    cpf <- recebeCPF

                    return (cpf, nome, dataNasc, sexo)

criaLista :: IO ListaPessoa
criaLista = return [("06601521345", "Jederilson", (20,7,1999), 'M')]

inserePessoa :: ListaPessoa -> Pessoa -> ListaPessoa
inserePessoa listaPessoas pessoa = pessoa : listaPessoas

buscaPessoa :: ListaPessoa -> CPF -> Pessoa
buscaPessoa [] cpf = error "Pessoa nao encontrada."
buscaPessoa ((cpfp, nome, dataNasc, sexo):r) cpf
    | cpf == cpfp = (cpfp, nome, dataNasc, sexo)
    | otherwise = buscaPessoa r cpf

checaEntrada :: Int -> Maybe Int
checaEntrada option
    | option >= 1 && option <= 4 = Just option
    | otherwise = Nothing

menu :: IO Int
menu = do
        print "Escolha uma das opcoes a seguir:"
        print "1. Cadastrar uma pessoa;"
        print "2. Apresentar todas as pessoas cadastradas;"
        print "3. Buscar uma pessoa pelo CPF;"
        print "4. Encerrar programa."
        putStr "Digite sua escolha: "
        option <- getLine
        return (read option :: Int)

aplicacao :: ListaPessoa -> IO ()
aplicacao lista = do
                    option <- menu
                    case checaEntrada option of
                        Nothing -> do 
                                    print "Entrada invalida"
                                    print "Tente novamente"
                                    aplicacao lista
                        Just 1 -> do 
                                    novaPessoa <- cadastraPessoa
                                    print(novaPessoa)
                                    aplicacao (inserePessoa lista novaPessoa)
                        Just 2 -> do
                                    print(lista)
                                    aplicacao lista
                        Just 3 -> do
                                    cpf <- recebeCPF
                                    print(buscaPessoa lista cpf)
                                    aplicacao lista
                        Just 4 -> print("Fim do programa.")

main :: IO ()
main = aplicacao []