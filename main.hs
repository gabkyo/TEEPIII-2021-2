import FileIO
import Operacoes
---------------------
-- Constantes

------------------

main = do
    --arquivos <- promptNomes  --le os nomes dos arquivos [entrada,saida] e K
    let arquivos = ["iris.csv"] --para teste
    dados <- lerEntrada (arquivos !! 0)
    let pontos = pontoInit dados
    print pontos
    let arvore = arvInit pontos 1
    putStrLn $ arvString arvore

