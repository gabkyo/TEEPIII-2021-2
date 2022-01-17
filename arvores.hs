module Arvores where

import Pontos
import Data.List

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
-- vetor arvore (p1,p2) onde p1 p2 sao potos ligados por terem a menor distancia entre eles

--Galhos----
type Galho = (Ponto,Ponto)

--igualdade entre galhos
galhoEq :: Galho -> Galho -> Bool
galhoEq a b = pontoTupleEq a b

--se um ponto esta em um galho
pontoGalhoEq :: Ponto -> Galho -> Bool
pontoGalhoEq a (b,c) = (pontoEq a b) || (pontoEq a c)

--aresta de um galho
galhoAresta :: Galho -> Double
galhoAresta galho = pontoDist  (fst galho) (snd galho) 



------------
--Arvore----
type Arvore = [Galho]

--se um ponto esta na arvore
arvPontoEq :: Arvore -> Ponto -> Bool
arvPontoEq arvore ponto = or $ map (pontoGalhoEq ponto) arvore

--se um galho esta na arvore
arvGalhoEq :: Arvore -> Galho -> Bool
arvGalhoEq arvore galho = or $ map (galhoEq galho) arvore

-- se um galho é valido para se juntar a arvore
-- ou seja tem um ponto que pertence a arvore e um que nao 
arvGalhoCheck :: Arvore -> Galho -> Bool
arvGalhoCheck arvore (a,b) = (arvPontoEq arvore a) /= (arvPontoEq arvore b)

--lista de pontos na arvore , nao ordenado
arvPontos :: Arvore -> [Ponto]
arvPontos [] = []
arvPontos arvore = do
    let lista = (map fst arvore) ++ (map snd arvore) --lista de todos os pontos
    nubBy pontoEq lista --remove duplicatas

-- maior aresta da arvore e galho com maior aresta
arvMaiorGalho :: Arvore -> (Double,Galho)
arvMaiorGalho arvore = do
    let arestas = [(pontoDist (fst x) (snd x), x) | x <- arvore]  -- (tamanho aresta, Par de pontos)
    maximum arestas --par com a maior aresta

-- pega todos os galhos ligados a um ponto
-- pegando primeiro todos os pontos ligados direto ao ponto de referencia
-- e aplicando recursivamente nos galhos nao obtidos
arvSeguePonto :: Arvore -> Ponto -> Arvore
arvSeguePonto arvore pRef = do
    let arvRef = filter (\x -> pontoGalhoEq pRef x) arvore -- parte da arvore com galhos ligado ao ponto atual
    let proximos_pontos = filter (pontoEq pRef) (arvPontos arvRef) -- pontos que estavam ligados ao atual
    let arv2 = arvore \\ arvRef --arvore sem os galhos ja obtidos 
    arvRef ++ (concatMap (\x -> arvSeguePonto arv2 x ) proximos_pontos) 
    --concatena os 
        --galhos atuais com os galhos obtidos dos pontos ligados na arvore sem  os galhos obtidos


--incia a montagem da arvore com o ponto de index dado
-- e manda pra arvBuild montar recursivamente
arvInit :: [Ponto] -> Int -> Arvore
arvInit [] _ = [] --arvore vazia
arvInit pontos index = do
    if not $ (pontoEqDim pontos) &&  (pontoUnico pontos)  
        then error $ "arvInit\nPontos Repetidos ou com dimensoes diferentes.\nPontos" ++ (show pontos)
    else if (length pontos) < 2 
        then error $ "arvInit\nNão há pontos o bastante.\nPontos" ++ (show pontos)
    else if (pontoI pontos index) == []
        then error $ "arvInit\nNão tem o index pedido, Index: " ++ (show index) ++ "\nPontos" ++ (show pontos)
    else do
        let raiz = pontoI pontos index
        let parRaiz = pontoMaisProximo pontos (head raiz)
        let galhoInicial = ((head raiz),parRaiz)
        let arvore = [galhoInicial]
        arvBuild pontos arvore
            

--cresce a arvore apos iniciada
arvBuild :: [Ponto] -> Arvore -> Arvore
arvBuild [] arvore = arvore -- sem pontos para adicionar
arvBuild _ [] = error "arvBuild\n Arvore não deve entrar vazia aqui" 
arvBuild pontos arvore = do
    let pontosArv = arvPontos arvore --pontos na arvore
    let pontosFora = pontos \\ pontosArv --pontos fora da arvore
    if pontosFora == []
        then arvore -- nao tem mais pontos a adicionar na arvore
    else do
        let novo_galho = pontoMaisProximoN pontosArv pontosFora  --novo galho
        arvBuild pontos (arvore ++ [novo_galho])


-- retorna a arvore como uma string "ponto, ponto ..."
arv2String :: Arvore -> String
arv2String arvore = do
    let pontos = sort $ arvPontos arvore --pontos na arvore
    let string_pontos = map (\x -> show (fst x) ) pontos -- 
    concat $ intersperse ", " string_pontos

------------
--Floresta--
type Floresta = [Arvore]

--dado uma floresta, retorna as strings das arvores divididas por \n
flo2String :: Floresta -> String
flo2String floresta = unlines $ map arv2String floresta

--retorna a arvore com a maior aresta
floArvMaiorAresta :: Floresta -> Arvore
floArvMaiorAresta floresta = do
    let maxArestas = map arvMaiorGalho floresta --[(aresta,galho)]
    let ranking = zip (map fst maxArestas) floresta  -- (maiorAresta,arvore)
    snd $ maximum ranking


--divide a arvore uma vez no maior galho, retornando uma floresta
arvSplit :: Arvore -> Floresta
arvSplit arv0 | (length arv0) < 2 = error $ "arvSplit\nArvore não é grande o bastante para dividir\nArvore:" ++ (arv2String arv0) 
arvSplit arv0 = do
    let galhoM = snd $ arvMaiorGalho arv0 --maior galho
    let arv1 = filter (== galhoM) arv0 --arv sem o maior galho
    [arvSeguePonto arv1 $ fst galhoM, arvSeguePonto arv1 $ snd galhoM]

-- da split na maior arvore da floresta 
{-
floSplit :: Floresta -> Floresta
floSplit floresta ka | ka < (length floresta) = error $ "floSplit\nFloresta já é maior que K=" ++ (show ka) ++ "\n Floresta:\n" ++ flo2String
    | ka == (length floresta) = floresta -- ja ta do tamanho certa
    | otherwise = do
        let arvoreMaior = floArvMaiorAresta floresta
        if (length arvoreMaior) < 2 --nao da pra dividir
            then if (length floresta) -}

        
----------------------


