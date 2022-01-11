module Operacoes where

-- arvore com os pontos onde a distancia entre nos é a distancia euclidiana sqrt(soma(quadrado(xik-xjk)))
-- a identificacao de cada linha é o numero da linha

-- ** eh exponenciacao float, ^ e  ^^ eh expo int


----------------------
-- DEFINICAO DOS PONTOS

data Ponto = Ponto { ind :: Int , dim :: [Double]} deriving (Show,Read,Eq)

instance Ord Ponto where
    compare a b = compare (ind a) (ind b)

--pega os pontos e monta uma lista de Ponto
pontoInit :: [[Double]] -> [Ponto] 
pontoInit pontos | (length pontos) > 0 = do
    let comprimento = length pontos
    let x = zip [1..comprimento] pontos
    map (\(i,p) -> Ponto { ind = i, dim= p}) x
 |otherwise = []

--mede a distancia entre 2 pontos
pontoDist :: Ponto -> Ponto -> Double
pontoDist p1 p2 | ((length $ dim p1) == (length $ dim p2)) = do
    let param = zip (dim p1) (dim p2) -- param = [(dimensao n de p1, dimensao n de p2)]
    let diff = map (\(x,y) -> (x-y) ** 2 ) param -- (dn1-dn2)^2
    (sum diff) ** 0.5
 | otherwise = error $ concat ["Pontos de dimensões diferentes: \n P1: ",show p1,"\nP2: ",show p2]

-- retorna o ponto mais proximo de um ponto que nao é ele mesmo
pontoMaisProximo :: Ponto -> [Ponto] -> Ponto
pontoMaisProximo pRef pontos = do
    let lista = filter (/= pRef) pontos
    let distancias = map (pontoDist pRef) lista
    snd $ minimum (zip distancias lista)

----------------------
----------------------
-- DEFINICAO DA ARVORE

{-
1. Escolher um ponto inicial para compor a árvore geradora mínima
2. Adicionar o ponto mais próximo a qualquer nó da árvores à árvore geradora mínima
3. Repetir o passo 2 até que todos os pontos tenham sido adicionados à árvore geradora
mínima
4. Escolher a maior aresta da árvore geradora mínima para dividi-la em dois grupos
5. Repetir o passo 4 para a floresta de árvores formadas até que se tenham apenas K
árvores (os K grupos).
-}

-- arvore , que pode ser ArvoreVazia ou  Node a com filhos
data Arvore a = ArvoreVazia | Node a [Arvore a] deriving (Show,Read,Eq) 

-- insere o ponto novo como filho do pai escolhido ou inicializa se arvore vazia
arvoreInsert :: (Eq a) => a -> a -> Arvore a -> Arvore a
arvoreInsert pnovo _ ArvoreVazia = Node pnovo []  -- arvore vazia pnovo e a raiz
arvoreInsert pnovo pai (Node a filhos) | a == pai = Node pai $ (Node pnovo []) : filhos -- no atual eh o procurado add pnovo como filho
    | otherwise = Node a $ map (arvoreInsert pnovo pai) filhos --no nao eh o procurado, aplica nos filhos

--profundidade da arvore
arvoreDepth :: Arvore a -> Int
arvoreDepth ArvoreVazia = 0
arvoreDepth (Node a filhos) = 1 + maximum (map arvoreDepth filhos)

-- verifica se o Node ja esta na arvore
arvoreTem :: (Eq a) => a -> Arvore a -> Bool
arvoreTem _ ArvoreVazia = False
arvoreTem pRef (Node a filhos)  = or ((a == pRef ) : (map (arvoreTem pRef) filhos) )

-- lista de Nodes na arvore
arvoreNodes :: Arvore a -> [a]
arvoreNodes ArvoreVazia = []
arvoreNodes (Node a filhos) = a : (concatMap arvoreNodes filhos)

--lista de nodes que nao estao na arvore mas estao na lista
arvoreNodesNot :: (Eq a) => [a] -> Arvore a -> [a]
arvoreNodesNot pontos arvore = do
    let regra = arvoreNodes arvore
    filter (\x ->notElem x regra) pontos

--construir a arvore
arvoreBuild :: [a] -> Arvore a -> Arvore a



----------------------

{-
1. Escolher um ponto inicial para compor a árvore geradora mínima
2. Adicionar o ponto mais próximo a qualquer nó da árvores à árvore geradora mínima
3. Repetir o passo 2 até que todos os pontos tenham sido adicionados à árvore geradora
mínima
4. Escolher a maior aresta da árvore geradora mínima para dividi-la em dois grupos
5. Repetir o passo 4 para a floresta de árvores formadas até que se tenham apenas K
árvores (os K grupos).
-}