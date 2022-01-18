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
    


--checa se todos os pontos tem o mesmo numero de dimensoes
checaVetorEntrada :: [[Double]] -> Bool
checaVetorEntrada entrada = do
    let tamanhos = map length entrada
    let ref = head tamanhos 
    all (== ref) tamanhos 

-- manda uma string pro terminal e envia a resposta de volta
perguntaUsuario :: String -> IO String 
perguntaUsuario linha = do
    putStrLn linha
    resposta <- getLine
    return resposta
-------------------------
--Saida
-------------------------

--escreve texto no arquivo nome , sobrescrevendo o que ja existe
escreveSaida :: String -> String -> IO ()
escreveSaida nome texto = writeFile nome texto
