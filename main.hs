import FileIO
import Operacoes


main = do
    --arquivos <- promptNomes  --le os nomes dos arquivos [entrada,saida] e K
    --dados <- lerEntrada (arquivos !! 0)
    dados <- lerEntrada "iris.csv"
    let pontos = pontoInit dados
    let p1 = head $ filter (\x -> (ind x) == 1) pontos
    print p1
