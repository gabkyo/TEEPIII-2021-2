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
 
--galho pode ser normal com dois pontos diferentes ou "raiz" com o mesmo ponto duas vezes
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

--arvore é um grupo de galhos
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
-- se for um galho raiz com o mesmo ponto , dist é 0 e nao vai ser o maior mesmo
arvMaiorGalho :: Arvore -> (Double,Galho)
arvMaiorGalho arvore = do
    let arestas = [(pontoDist (fst x) (snd x), x) | x <- arvore]  -- (tamanho aresta, Par de pontos)
    maximum arestas --par com a maior aresta

--insere galho na arvore
arvGalhoIns :: Arvore -> Galho -> Arvore
arvGalhoIns [] galho = [galho]
arvGalhoIns arvore galho = if not $ arvGalhoCheck arvore galho -- se nao sao pontos um de fora um de dentro
        then arvore
    else galho : (filter (\x -> (x /= (fst galho,fst galho)) && (x /= (snd galho,snd galho)) ) arvore) 
    --filtra galho com nos iguais agora que tem um galho novo com nos diferentes pois a raiz e a quebra do ultimo ramo gera arvore com um ponto e um galho do mesmo ponto

-- insere ponto na arvore formando galho com o mais proximo da arvore
arvPontoIns :: Arvore -> Ponto -> Arvore
arvPontoIns [] ponto = [(ponto,ponto)] -- raiz é galho com no repetido
arvPontoIns arvore ponto = if arvPontoEq arvore ponto -- ja tem
        then arvore -- nao muda
    else do
        let par = (pontoMaisProximo (arvPontos arvore) ponto)
        arvGalhoIns arvore (ponto,par)

--remove ponto e deixa os galhos ligados ao ponto como raizes (p1,p1)
arvPontoDel :: Arvore -> Ponto -> Arvore
arvPontoDel arvore ponto =  do
    let pontos_desconectados = map (\(x,y) -> if x == ponto then (y,y) else if y == ponto then (x,x) else (x,y)) arvore --faz os elementos com o ponto serem raizes
    filter (\x -> not (pontoGalhoEq ponto x)) pontos_desconectados --remove raizes do ponto escolhido

--remove galho e deixa galhos ligados como raizes (p1,p1)
arvGalhoDel :: Arvore -> Galho -> Arvore
arvGalhoDel arvore galho = do
    let arv1 = arvPontoDel arvore (fst galho)
    arvPontoDel arv1 (snd galho)



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
    else if (pontoI pontos index) == []
        then error $ "arvInit\nNão tem o index pedido, Index: " ++ (show index) ++ "\nPontos" ++ (show pontos)
    else do
        let raiz = head $ pontoI pontos index -- raiz da arvore
        arvBuild pontos (arvPontoIns [] raiz) -- continua a construir
            

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
        arvBuild pontos (arvGalhoIns arvore novo_galho)


-- retorna a arvore como uma string "ponto, ponto ..."
arv2String :: Arvore -> String
arv2String arvore = do
    let pontos = sort $ arvPontos arvore --pontos na arvore
    let string_pontos = map (\x -> show (fst x) ) pontos -- 
    concat $ intersperse ", " string_pontos

------------
--Floresta--

--floresta é um grupo de arvores
type Floresta = [Arvore]

--dado uma floresta, retorna as strings das arvores divididas por \n
flo2String :: Floresta -> String
flo2String floresta = unlines $ map arv2String (sort floresta)

--retorna a arvore com a maior aresta
floArvMaiorAresta :: Floresta -> Arvore
floArvMaiorAresta floresta = do
    let maxArestas = map arvMaiorGalho floresta --[(aresta,galho)]
    let ranking = zip (map fst maxArestas) floresta  -- (maiorAresta,arvore)
    snd $ maximum ranking

--divide a arvore uma vez no maior galho, retornando uma floresta
arvSplit :: Arvore -> Floresta
arvSplit arv0 = if (length arv0) <= 1
        then if (length (arvPontos arv0) <= 1) 
            then error $ "arvSplit\nArvore não é grande o bastante para dividir\nArvore:" ++ (arv2String arv0)
        else do
            let galho = arv0 !! 0 
            [[(fst galho,fst galho)],[(snd galho,snd galho)]]
    else do --arvore é grande o bastante
        let galhoM = snd $ arvMaiorGalho arv0 --maior galho
        let arv1 = arvGalhoDel arv0 galhoM --arv sem os galhos ligados ao maior galho
        [[galhoM],arv1]

-- da split na maior arvore da floresta ate o numero de arvores >= ka
floSplit :: Floresta -> Int -> Floresta
floSplit floresta ka = if ka < 0 
        then error $ "floSplit\nK negativo"
    else if ka <= (length floresta) 
        then floresta --ja ta com tamanho o bastante
    else do --escolhe arvore
        let arvore = floArvMaiorAresta floresta --maior arvore da floresta
        let nova_floresta = (filter (/= arvore) floresta) ++ (arvSplit arvore) -- as outras arvores mais a maior arvore dividida
        floSplit nova_floresta ka



        
----------------------


