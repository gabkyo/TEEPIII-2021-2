module Pontos where

import Data.List

----------------------
-- Lista de diferencas

--elementos que estao em manter mas nao em remover
difList :: Eq a => [a] -> [a] -> [a]
difList remover manter = filter (\x -> notElem x remover) manter

-- todos os elementos distintos
difListAll :: Eq a => [a] -> [a] -> [a]
difListAll a b = (difList a b) ++ (difList b a)

--retorna true se uma das listas esta contida na outra
isSubList :: Eq a => [a] -> [a] -> Bool
isSubList a b = ((difList a b) == []) ||  ((difList b a) == [])

--retorna lista de elementos duplicados
duplicados :: Eq a => [a] -> [a]
duplicados lista = [ lista !! y |  y <- [0..((length lista)-1)] , x <- [0..y] , x /= y, (lista !! x) == (lista !! y) ]

--retorna lista de cada elemento que ocorre
uniqueList :: Eq a => [a] -> [a]
uniqueList lista = do
    let dupli = duplicados lista
    let unicos = filter (\x -> notElem x dupli) lista
    dupli ++ unicos

--sorteia do menor pro maior
sortMin :: Ord a => [a] -> [a]
sortMin [] = []
sortMin lista = do
    let x = minimum lista
    x : (sortMin (filter (/= x) lista))

sortMax :: Ord a => [a] -> [a]
sortMax lista = reverse (sortMin lista)
----------------------
--Outras funções------

--se a lista de lista de doubles tem elementos com a mesma dimensao
doubleEqDim :: [[Double]] -> Bool
doubleEqDim lista = and $ map (\x -> (length x) == (length (head lista))) lista

-- ** eh exponenciacao float, ^ e  ^^ eh expo int

--distancia euclidiana entre duas listas de dimensoes do mesmo tamanho
distEuclid :: [Double] -> [Double] -> Double
distEuclid a b = if (length a) /= (length b)
        then error $ "distEuclid\nElementos nao tem o mesmo tamanho\n" ++ (show a) ++ "\n" ++ (show b)
    else do
        let lista = zip a b
        let diff = map (\(x,y) -> (x-y) ** 2 ) lista --(xi1 - xj1) ^2
        (sum diff) ** 0.5  --raiz da soma
    

-- zip [0..] lista = [(0,a), (1,b) .. ]

----------------------
-- DEFINICAO DOS PONTOS

-- Ponto é (Index>0,Dimensoes)
type Ponto = (Int,[Double])
--fst ponto é o indice e snd ponto as dimensoes

--se os indices sao iguais os pontos sao iguais
pontoEq :: Ponto -> Ponto -> Bool
pontoEq a b = (fst a) == (fst b)

--se uma tupla de pontos é equivalente
pontoTupleEq :: (Ponto,Ponto) -> (Ponto,Ponto) -> Bool
pontoTupleEq (a,b) (c,d) = ((pontoEq a c) && (pontoEq b d)) || ((pontoEq a d) && (pontoEq b c))

--transforma lista de double em ponto 
pontoInit :: [[Double]] -> [Ponto] 
pontoInit lista = if not (doubleEqDim lista) 
        then error $ "pontoInit\nElementos nao tem o mesmo tamanho\n" ++ show (lista)
    else zip [1..] lista

--mede a distancia euclidiana entre 2 pontos
pontoDist :: Ponto -> Ponto -> Double
pontoDist (a,b) (c,d) = distEuclid b d

-- retorna o ponto mais proximo da lista de um ponto que nao é ele mesmo
pontoMaisProximo :: [Ponto] -> Ponto -> Ponto
pontoMaisProximo lista pRef = snd $ minimum [(pontoDist pRef x, x) | x <- lista, (fst x) /= (fst pRef) ]

--retorna os pontos mais proximos entre duas listas que nao seja o mesmo ponto
pontoMaisProximoN :: [Ponto] -> [Ponto] -> (Ponto,Ponto)
pontoMaisProximoN a b = do
    let pares = [(pontoDist x y, (x,y)) | x <- a, y <- b , x /= y] --(dist, (par de pontos))
    snd $ minimum pares

--retorna os pontos com index I
pontoI :: [Ponto] -> Int -> [Ponto]
pontoI lista index = filter (\x -> index == (fst x)) lista

--retorna se a lista de pontos tem indices unicos
pontoUnico :: [Ponto] -> Bool
pontoUnico lista =do
    let indices = map fst lista --lista de indices nos pontos
    let ocorrencias = map (\x -> filter (==x) indices) indices --quantas vezes cada indice aparece
    let unicos = map (\x -> (length x) == 1) ocorrencias -- se todos so aparecem uma vez
    and unicos

--se todos os pontos tem o mesmo numero de dimensoes
pontoEqDim :: [Ponto] -> Bool
pontoEqDim lista = doubleEqDim (map snd lista)
    