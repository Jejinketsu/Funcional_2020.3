module QuickSort where
import Data.Char

type Time = (String, String, String, Int)

quickSort :: [Time] -> [Time]
quickSort [] = []
quickSort ((nome, estado, pais, ano_fundacao):r) = quickSort parte_esquerda  ++ [(nome, estado, pais, ano_fundacao)] ++ quickSort parte_direita
    where
        parte_esquerda = [(n, e, p, a) | (n, e, p, a)  <- r, transformaMinusculo(n) < transformaMinusculo(nome)]
        parte_direita = [(n, e, p, a)  |(n, e, p, a)  <- r, transformaMinusculo(n) >= transformaMinusculo(nome)]
        --Onde n = nome; e = estado; p = pais; a = ano de publicação

lerTime :: IO Time
lerTime = do
        putStr "Digite o nome do Clube >> "
        nome <- getLine
        putStr "Digite o estado do Clube >> "
        estado <- getLine
        putStr "Digite o pais do Clube >> "
        pais <- getLine
        putStr "Digite o ano de fundacao >> "
        ano_fundacao <- getLine
        
        return (nome, estado, pais, read ano_fundacao :: Int)

imprime :: [Time] -> IO ()
imprime([]) = putStr "\n"
imprime((nome, estado, pais, ano_fundacao):r) = do
                            putStr "Nome: " >> putStr(nome)
                            putStr "   |   Estado: " >> putStr(estado)
                            putStr "   |   Pais: " >> putStr(pais)
                            putStr "   |   Ano de Fundacao: " >> putStr(show ano_fundacao)
                            putStr "\n"
                            imprime(r)

buscaTime :: (String, [Time]) -> IO()
buscaTime(nomeTime, []) = putStr "Time nao Encontrado"
buscaTime(nomeTime, (nome, estado, pais, ano_fundacao):r)
    |transformaMinusculo(nomeTime) == transformaMinusculo(nome) = imprime([(nome, estado, pais, ano_fundacao)])
    | otherwise = buscaTime(nomeTime, r)

transformaMinusculo :: (String) -> String
transformaMinusculo (lista) = map(\x -> toLower x) lista

checaEntrada :: Int -> Maybe Int
checaEntrada option
    | option >= 1 && option <= 5 = Just option
    | otherwise = Nothing

menu :: IO Int
menu = do
        putStr "\n"
        print("[*-----------------------------------*]")
        print "| 1 - Cadastrar Time                  |"
        print "| 2 - Ordenar                         |"
        print "| 3 - Listar Todos                    |"
        print "| 4 - Buscar                          |"
        print "| 5 - Finalizar                       |"
        print("[*-----------------------------------*]")
        putStr "\nDigite uma opcao >> "
        op <- getLine
        return (read op :: Int)

aplicacao :: [Time] -> IO ()
aplicacao listaTime = do
                    option <- menu
                    case checaEntrada option of
                        Nothing -> do 
                                    print "Entrada invalida"
                                    print "Tente novamente"
                                    aplicacao listaTime
                        Just 1 -> do
                                    novoTime <- lerTime
                                    aplicacao (novoTime : listaTime)
                        Just 2 -> do 
                                    aplicacao (quickSort listaTime)
                        Just 3 -> do
                                    imprime(listaTime)
                                    aplicacao listaTime
                        Just 4 -> do
                                    putStr "Digite um nome: "
                                    nomeTime <- getLine
                                    buscaTime(nomeTime, listaTime)
                                    aplicacao listaTime
                        Just 5 -> print("Fim do programa.")

main :: IO ()
main = aplicacao []         