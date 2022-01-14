import FileIO
import Arvores
---------------------
-- Constantes

------------------

main = do
    --arquivos <- promptNomes  --le os nomes dos arquivos [entrada,saida] e K
    dados <- lerEntrada (arquivos !! 0)
    let pontos = pontoInit dados
    print pontos
    let arvore = arvInit pontos 1
    putStr $ arvString arvore
    print $ length (arvPontos arvore)

