module FileIO where

import System.IO

-------------------------
--Entrada
-------------------------

--quebra a string em tokens nos delimitadores e retorna array sem os delims
quebraDelim :: Char -> String -> [String]
quebraDelim caractere linha = case break (== caractere) linha of
    (a,caractere:b) -> a: quebraDelim caractere b
    (a, _) -> [a]

-- pega uma string e le as n dimensoes do ponto, onde o divisor eh ',' e decimal eh '.'
lePonto :: String -> [Double]
lePonto linha = do
    let tokens = filter (/="") $ quebraDelim ',' linha
    map read tokens :: [Double]
    

-- Le os nomes dos arquivos de entrada, saida e K
-- e retorna como array de strings
promptNomes :: IO [String] 
promptNomes = do
    putStrLn "Forneca o nome do arquivo de entrada:"
    entrada <- getLine
    putStrLn "Forneca o nome do arquivo de saida:"
    saida <- getLine
    putStrLn "Forneca o número de grupos (K):"
    grupos <- getLine
    return [entrada,saida,grupos]

--checa se todos os pontos tem o mesmo numero de dimensoes
checaVetorEntrada :: [[Double]] -> Bool
checaVetorEntrada entrada = do
    let tamanhos = map length entrada
    let ref = head tamanhos 
    all (== ref) tamanhos 

-- le a entrada ex: nome = "teste.txt" e retorna array dos pontos,
-- reto
lerEntrada :: String -> IO [[Double]]
lerEntrada nome = do
    texto <- readFile nome
    let linhas = lines texto --array com as linhas do arquivo
    let pontos = map lePonto linhas
    if (checaVetorEntrada pontos) 
        then (return pontos)
        else do
            putStrLn "ERRO: Pontos de entrada não foram lidos com a mesma dimensão."
            return [[]]
    --pontos <- (map lePonto linhas)
    --return pontos
-------------------------
--Saida
-------------------------