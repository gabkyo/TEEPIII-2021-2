module Cidades where

import Data.List
import Data.Maybe 
import Text.Printf (printf)


data Cidade = Cidade{ ind :: Int , nome :: String , coord :: [Double], diaria :: Double , custos :: [Double] } deriving Show

--gera uma representacao em string da cidade
cidadeString :: Cidade -> String
cidadeString (Cidade ind nome coord diaria custos)= do
    "Cidade " ++ (show ind) ++ ": " ++  nome ++ " Pos:" ++ (show coord) ++ " Diaria:" ++ (show diaria) ++ " Custos:" ++ (show custos)

--gera uma string com todas as cidades separadas por \n
cidadeStringN :: [Cidade] -> String
cidadeStringN cidades= unlines $ map cidadeString cidades



--distancia entre as cidades a e b
cidadesDistancia :: Cidade -> Cidade -> Double
cidadesDistancia a b = do
    let valores = zip (coord a) (coord b)
    let diferencas = map (\(x,y) -> ( subtract (max x y) (min x y) )^2) valores
    sqrt (sum diferencas)


--distancia entre as cidades com indices i j
cidadesDistanciaInd :: [Cidade] -> Int -> Int -> Double
cidadesDistanciaInd cidades i j = do
    let mci = find (\x -> (ind x) == i) cidades
    let mcj = find (\x -> (ind x) == j) cidades 
    if (isNothing  mci ) || (isNothing mcj)
        then error $ "cidadesDistanciaInd\nNão existe alguma das cidades com indice pedido\nIndices: " ++ (show i) ++ " "++ (show j) ++ "\nCidades:\n" ++ (show cidades)
        else do
            let ci = fromJust mci
            let cj = fromJust mcj
            cidadesDistancia ci cj


--custo de viagem e diarias entre duas cidades
cidadesCV :: Cidade -> Cidade -> Int -> Double
cidadesCV ci cj noites = do
    let custoij = (custos ci) !! ((ind cj)-1)
    (cidadesDistancia ci cj) * custoij + (diaria cj)*(fromIntegral noites)


--lista de nomes da cidade formatados como pedido
cidadesSequenciaString :: [Cidade] -> String
cidadesSequenciaString cidades = do
    let nomes = concat $ intersperse "," $ map (\x -> "\"" ++ (nome x) ++ "\"") cidades
    "Sequencia de Cidades:[" ++ nomes ++ "] "


--viaja pelas cidades na ordem que estao na lista dada, retorna distancia total e custo total para uma noite em cada
cidadesViagemSeq1 :: [Cidade] -> (Double,Double)
cidadesViagemSeq1 cidades = do
    let duplas = zip (init cidades) (tail cidades)
    let distancia = sum $ map (\(x,y) -> cidadesDistancia x y) duplas
    let diarias = sum $ map (\(x,y) -> cidadesCV x y 1) duplas
    (distancia,diarias)


--sort para (distancia,Cidade) a menor distancia primeiro , desempata pelo maior indice
sortOrdemSuperior :: (Double, Cidade) -> (Double,Cidade) -> Ordering
sortOrdemSuperior (a,b) (c,d) | a==c = compare (ind d) (ind b) --invertido pois o maior indice primeiro
    |otherwise = compare a c -- menor distancia primeiro


--aplica recursivamente a ordenacao por ordem superior
cidadesOrdemSuperior :: [Cidade] -> [Cidade] -> [Cidade]
cidadesOrdemSuperior ordenados [] = ordenados
cidadesOrdemSuperior [] lista = do
    let teste = find (\x -> 1 == ind x) lista
    if isNothing teste
        then error $ "cidadesOrdemSuperior\nNão existe cidade com indice pedido\nIndice: 1\nCidades:\n" ++ (show lista)
    else do
        let ci = [fromJust teste]
        cidadesOrdemSuperior ci lista
cidadesOrdemSuperior ordenados lista = do
    let ultimo = last ordenados
    let candidatos = filter (\x -> (ind ultimo) < (ind x)) lista
    if null candidatos 
        then ordenados
    else do
        let valores = [(cidadesDistancia ultimo x, x) | x <- candidatos]
        let ordNovo = ordenados ++ [snd (minimumBy sortOrdemSuperior valores)]
        cidadesOrdemSuperior ordNovo lista


--aplica recursivamente a ordenacao por menor custo primeiro, desempata por  maior indice
cidadesCustoInferior :: [Cidade] -> [Cidade] -> [Cidade]
cidadesCustoInferior ordenados [] = ordenados
cidadesCustoInferior [] lista = do
    let teste = find (\x -> 1 == ind x) lista
    if isNothing teste
        then error $ "cidadesCustoInferior\nNão existe cidade com indice pedido\nIndice: 1\nCidades:\n" ++ (show lista)
    else do
        let ci = [fromJust teste]
        cidadesCustoInferior ci lista
cidadesCustoInferior ordenados lista = do
    let ultimo = last ordenados
    let candidatos = filter (\x -> (ind ultimo) < (ind x)) lista
    if null candidatos 
        then ordenados
    else do
        let valores = [(cidadesCV ultimo x 1, x) | x <- candidatos]
        let ordNovo = ordenados ++ [snd (minimumBy sortOrdemSuperior valores)]
        cidadesCustoInferior ordNovo lista


--transforma double em uma string com n digitos
double2StrPrec :: Double -> Int -> String
double2StrPrec numero n = do
    let expo = until (\x -> 10^n <=  numero * x) (*10) 1
    show $ (fromIntegral (round (expo * numero)))/expo


-- string do pedido no item 3
item3 :: [Cidade] -> String 
item3 cidades = do
    let valores = (\(x,y) -> (double2StrPrec x 8, double2StrPrec y 8)) $ cidadesViagemSeq1 cidades
    "Distancia: " ++ (fst valores) ++ " Custo: " ++ (snd valores)

-- string do pedido no item 4
item4 :: [Cidade] -> String
item4 cidades = do
    let ordenado = cidadesOrdemSuperior [] cidades
    (cidadesSequenciaString ordenado) ++ (item3 ordenado)

-- string do pedido no item 4
item5 :: [Cidade] -> String
item5 cidades = do
    let ordenado = cidadesCustoInferior [] cidades
    (cidadesSequenciaString ordenado) ++ (item3 ordenado)
