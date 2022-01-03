import FileIO
import Operacoes


main = do
    arquivos <- promptNomes  --le os nomes dos arquivos [entrada,saida] e K
    pontos <- lerEntrada (arquivos !! 0)
    print pontos
    