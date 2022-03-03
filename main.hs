import FileIO
import Arvores
import Pontos
---------------------
-- Constantes

------------------

main = do
    putStrLn "Forneca o nome do arquivo de entrada:"
    entrada <- getLine
    putStrLn "Forneca o nome do arquivo de saida:"
    saida <- getLine
    putStrLn "Forneca o número de grupos (K):"
    grupos <- getLine
    let ka = read grupos :: Int
    texto <- readFile entrada
    let dados = map lePonto (lines texto)
    let pontos = pontoInit dados
    let arvore = arvInit pontos 1
    let floresta = floSplit [arvore] ka
    putStrLn "Agrupamentos:"
    putStrLn $ flo2String floresta
    escreveSaida saida (flo2String floresta)

