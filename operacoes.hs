module Operacoes where

import Data.Tree
import Data.List

-- arvore com os pontos onde a distancia entre nos é a distancia euclidiana sqrt(soma(quadrado(xik-xjk)))
-- a identificacao de cada linha é o numero da linha

-- ** eh exponenciacao float, ^ e  ^^ eh expo int

{-
1. Escolher um ponto inicial para compor a árvore geradora mínima
2. Adicionar o ponto mais próximo a qualquer nó da árvores à árvore geradora mínima
3. Repetir o passo 2 até que todos os pontos tenham sido adicionados à árvore geradora
mínima
4. Escolher a maior aresta da árvore geradora mínima para dividi-la em dois grupos
5. Repetir o passo 4 para a floresta de árvores formadas até que se tenham apenas K
árvores (os K grupos).
-}


-- a distancia entre dois pontos é a raiz da soma dos quadrados das diferencas (soma((ai-bi)**2 + (aj-bj)**2 +  ...))**1/2
distEuclid :: [Double] -> [Double] -> Double
distEuclid p1 p2 | (length p1) == (length p2) = do
    let dimensoes = zip p1 p2
    sum (map (\(x,y) -> (x-y)**2 ) dimensoes) ** 1/2
 |otherwise = -1 --se der negativo eh pq houve erro

--distEuclid mas usando indexacao 1
distEuclidIndex :: [[Double]] -> Int -> Int -> Double 
distEuclidIndex pontos i1 i2 | (i1 > 0) && (i2 > 0 ) =  distEuclid (pontos !! (i1 -1)) (pontos !! (i2 -1))

-- retorna o ponto com menor distancia de um ponto de index iref
menorDist1P :: Int -> [[Double]] -> Int
menorDist1P iref pontos = do
    let tamanho = (length pontos) --numero de pontos
    let index = filter (/= iref) [1..tamanho] --index dos pontos que nao sao o de referencia
    let distancias = map (\x -> distEuclidIndex pontos iref x ) index --mapeia as distancias
    snd (minimum (zip distancias index)) --retorna o ponto com menor distancia e menor indice

--o minimo dado varios pontos de referencia
menorDistNP :: [Int] -> [[Double]] -> Int
menorDistNP arvore pontos = do
    let tamanho = (length pontos) --numero de pontos
    let index = [1..tamanho] \\ arvore--index dos pontos que nao sao os de referencia

snd (minimum (zip (\x -> map )))
    


-- passo 1 é adicionar o primeiro ponto a arvore
inicilizaArvore :: [Int]
inicilizaArvore = [1]

[(3,1),(2,2),(1,3)]