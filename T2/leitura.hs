module Leitura where

import System.IO
import Data.List
import Cidades

--Nome dos Arquivos
cidadesTxt = "nome-coord.txt"
diariasTxt = "diaria-custo.txt"
saidaTxt = "saida.txt"
--Numero Esperado de Dimensões
ndim = 2

checkCoord :: [[Double]] -> Bool --cordenadas devem ser [[x,y]] 
checkCoord coord = and $ [(length x) == ndim | x <- coord ]

checkCustos :: [[Double ]] -> Bool  --custos devem ser n arrays de n valores
checkCustos custos = do
    let tamanho = length custos
    let tamN = map length custos
    and $ map (tamanho == ) tamN

arqLinhas :: String -> [String]
arqLinhas arquivo = do
    let texto = readFile arquivo
    lines arquivo

leCidades :: [String] -> [(String, [Double])]
leCidades linhas | odd (length linhas) = error ("leCidades\nNumero impar de linhas em " ++ cidadesTxt ++ ", não há pares para todas as cidades\\coordenadas.\n" ++ (show linhas)) 
    | otherwise = do
        let ind = zip [1..(length  linhas)] linhas
        let coordenadas = [ map read (words (snd x)) ::[Double] | x <- ind, even (fst x)]
        if not (checkCoord coordenadas) 
            then error ("leCidades\nNumero de dimensoes lidas nao eh "++ (show ndim) ++ " para todos os elementos\n" ++ (show coordenadas))
        else do
            let nomes = [(snd x) | x <- ind, odd (fst x)]
            zip nomes coordenadas
    
leDiaria :: [String] -> [([Double], [Double])]
leDiaria linhas | odd (length linhas ) = error ("leDiaria\nNumero impar de linhas em " ++ diariasTxt ++ ", não há diarias\\custos para todas as cidades.\n" ++ (show linhas)) 
    | otherwise = do
        let palavras =  map words linhas --cada numero em string
        let valores = map (\x -> map read x ::[Double]) palavras --numeros em [[double]]
        let tamanho = length linhas -- numero de linhas 
        let diarias = take (div tamanho 2) valores -- pega a primeira metade que é diarias
        if not $ and (map (\x -> 1 == length x) diarias) --se as diarias não sao unitarias
            then error ("leDiaria\nDiarias mal formatadas / nao unitarias" ++ (show diarias))
        else do 
            let custos = valores \\ diarias -- pega a segunda metade
            if not (checkCustos custos)
                then error ("leDiaria\nMatriz de custos nao eh quadrada em " ++ diariasTxt ++ "\n" ++ (foldl (\x y -> x ++ (show y) ++ "\n") "" custos))
            else do
                zip diarias custos --faz pares de diaria e custos para cada cidade



listaCidades :: [(String, [Double])] -> [([Double], [Double])] -> [Cidade]
listaCidades cidades dc = do
    let indexes = [0.. ((length cidades)-1)]
    let nomes = map fst cidades
    let coord = map snd cidades
    let diaria = map (\x -> head (fst x)) dc
    let custos = map (snd) dc
    [Cidade{ ind = (x+1), nome = (nomes !! x), coord =(coord !! x), diaria = (diaria !! x), custos = (custos !! x) }| x <- indexes]



