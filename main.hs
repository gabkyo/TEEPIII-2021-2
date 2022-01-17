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
    texto <- readFile entrada
    let dados = map lePonto (lines texto)
    let pontos = pontoInit dados
    print pontos
    let arvore = arvInit pontos 1
    putStrLn $ arv2String arvore

