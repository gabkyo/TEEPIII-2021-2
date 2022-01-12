module Operacoes where

-- arvore com os pontos onde a distancia entre nos é a distancia euclidiana sqrt(soma(quadrado(xik-xjk)))
-- a identificacao de cada linha é o numero da linha

-- ** eh exponenciacao float, ^ e  ^^ eh expo int

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
----------------------

----------------------
-- DEFINICAO DOS PONTOS

-- ponto tem um index que comeca em 1 e uma lista com as dimensoes
data Ponto = Ponto { ind :: Int , dim :: [Double]} deriving (Show,Read,Eq,Ord)

--pega os pontos e monta uma lista de Ponto
pontoInit :: [[Double]] -> [Ponto] 
pontoInit pontos | (length pontos) > 0 = do
    let comprimento = length pontos
    let x = zip [1..comprimento] pontos
    map (\(i,p) -> Ponto { ind = i, dim= p}) x
 |otherwise = []

--mede a distancia euclidiana entre 2 pontos
pontoDist :: Ponto -> Ponto -> Double
pontoDist p1 p2 | ((length $ dim p1) == (length $ dim p2)) = do
    let param = zip (dim p1) (dim p2) -- param = [(dimensao n de p1, dimensao n de p2)]
    let diff = map (\(x,y) -> (x-y) ** 2 ) param -- (dn1-dn2)^2
    (sum diff) ** 0.5
 | otherwise = error $ concat ["pontoDist\n","Pontos de dimensões diferentes: \n P1: ",show p1,"\nP2: ",show p2]

-- retorna o ponto mais proximo da lista de um ponto que nao é ele mesmo
-- da erro se nao há nenhum 
pontoMaisProximo :: [Ponto] -> Ponto -> Ponto
pontoMaisProximo pontos pRef = do
    let lista = filter (/= pRef) pontos
    if lista == []
        then error $ "pontoMaisProximo\nNenhum ponto válido. Pref:" ++ (show pRef) ++ "\nPontos" ++ (show pontos)
    else do
        let distancias = map (pontoDist pRef) lista
        snd (minimum (zip distancias lista))

--retorna os pontos mais proximos entre duas listas que nao seja o mesmo ponto
-- da erro se há nenhum
pontoMaisProximoN :: [Ponto] -> [Ponto] -> (Ponto,Ponto)
pontoMaisProximoN a b = do
    if (a == []) || (b == []) 
        then error $ "pontoMaisProximoN\nLista Vazia.\n1a lista: " ++ (show a) ++ "\n2a lista: " ++ (show b)
    else do
        let lista = [(pontoDist x y,(x,y)) | x <- a , y <- b , x /= y] --pares de pontos distintos e a dist entre eles
        if lista == []
            then error $ "pontoMaisProximoN\nNenhum ponto válido. 1a lista:" ++ (show a) ++ "\n2 lista" ++ (show b)
        else snd $ minimum lista




-- retorna pontos da lista com o index dado ou []
pontoFindInd :: [Ponto] -> Int -> [Ponto]
pontoFindInd pontos index = filter (\x -> (ind x) == index) pontos

-- true se tem ponto duplicado   
pontoDuplicado :: [Ponto] -> Bool
pontoDuplicado pontos = do
    let indices = map ind pontos --lista de indices nos pontos
    let ocorrencias = map (\x -> filter (==x) indices) indices --quantas vezes cada indice aparece
    let repetidos = map (\x -> (length x) > 1) ocorrencias -- se algum tem mais que uma ocorrencia
    or repetidos 

--checa se pontos da lista tem a mesma quantidade de dimensoes
pontoEqDim :: [Ponto] -> Bool
pontoEqDim pontos = do
    let qDim = map (\x -> length (dim x)) pontos
    let eqDim = map (== (head qDim)) qDim
    and eqDim

--checa lista de pontos e da erro se nao estiver certa
pontoCheckAll :: [Ponto] -> Bool
pontoCheckAll pontos = if pontoDuplicado pontos
        then error $ "pontoCheckAll\nPontos tem duplicatas.\n" ++ (show pontos)
    else if pontoEqDim pontos
        then True
    else error ( "pontoCheckAll\nPontos tem dimensões diferentes.\n" ++ (show pontos) )

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
-- vetor arvore (p1,p2) onde p1 p2 sao potos ligados por terem a menor distancia entre eles

type Galho = (Ponto,Ponto)
type Arvore = [Galho]

--comparação entre galhos
galhoEq :: Galho -> Galho -> Bool
galhoEq (a,b) (c,d) =( (a==c) && (b==d) ) || ( (a==d) && (b==c) )

--se um ponto esta em um galho
pontoGalhoEq :: Ponto -> Galho -> Bool
pontoGalhoEq a (b,c) = (a == b) || (a == c)

--se um ponto esta na arvore
arvPontoEq :: Ponto -> Arvore -> Bool
arvPontoEq ponto arvore = or $ map (pontoGalhoEq ponto) arvore

--se um galho esta na arvore
arvGalhoEq :: Galho -> Arvore -> Bool
arvGalhoEq galho arvore = or $ map (galhoEq galho) arvore

-- se um galho é valido para se juntar a arvore
-- ou seja tem um ponto que pertence a arvore e um que nao 
arvGalhoCheck :: Arvore -> Galho -> Bool
arvGalhoCheck arvore galho = ((arvPontoEq (fst galho) arvore) && (not $ arvPontoEq (snd galho) arvore)) || 
    ((not $ arvPontoEq (fst galho) arvore) && ( arvPontoEq (snd galho) arvore))

--lista de pontos na arvore , nao ordenado
arvPontos :: Arvore -> [Ponto]
arvPontos [] = []
arvPontos arvore = do
    let lista = (map fst arvore) ++ (map snd arvore)
    uniqueList lista

-- galho com maior aresta da arvore
arvMaiorAresta :: Arvore -> Galho
arvMaiorAresta arvore = do
    let arestas = [(pontoDist (fst x) (snd x), x) | x <- arvore]  -- (tamanho aresta, Par de pontos)
    snd $ maximum arestas --par com a maior aresta

-- pega todos os galhos ligados a um ponto
arvSeguePonto :: Arvore -> Ponto -> Arvore
arvSeguePonto arvore pRef = do
    let arvRef = filter (\x -> pontoGalhoEq pRef x) arvore --galhos ligado ao ponto atual
    let proximos_pontos = filter (/= pRef) (arvPontos arvRef) --pontos que estavam ligados ao atual
    let arv2 = difList arvRef arvore --arvore sem os galhos ja obtidos 
    arvRef ++ (concatMap (\x -> arvSeguePonto arv2 x ) proximos_pontos) 
    --concatena os 
        --galhos atuais com os galhos obtidos dos pontos ligados na arvore sem  os galhos obtidos

--divide a arvore em dado galho e  retorna arvore velha sem os galhos conectados e arvore nova
arvDividir :: Arvore -> Galho -> (Arvore,Arvore) 
arvDividir arvore galho | not (arvGalhoEq galho arvore) = (arvore,[]) --galho nao esta na arvore logo arvore nova e nula
    | otherwise = do
        let arv_temp = filter (== galho) arvore --arvore sem o galho
        let arv1 = arvSeguePonto arv_temp (fst galho)
        let arv2 = arvSeguePonto arv_temp (snd galho)
        (arv1,arv2)

galhoString :: Galho -> String
galhoString galho =  "Galho (" ++ (show $ ind (fst galho)) ++ "," ++ (show $ ind (snd galho)) ++ ")\n"

arvString :: Arvore -> String
arvString arvore = "Árvore\n" ++ (concatMap galhoString arvore)

--incia a montagem da arvore
arvInit :: [Ponto] -> Int -> Arvore
arvInit [] _ = [] --arvore vazia
arvInit pontos index | index < 1 = error $ "arvInit\nIndex inválido, deve ser >1. Ind:"  ++ (show index)
    | otherwise = do
        if pontoDuplicado pontos
            then error $ "pontoCheckAll\nPontos tem duplicatas.\n" ++ (show pontos)
        else if not $ pontoEqDim pontos
            then error ( "pontoCheckAll\nPontos tem dimensões diferentes.\n" ++ (show pontos) ) 
        else do
            let p1 = pontoFindInd pontos index
            if (p1 == [])
                then error $ "arvInit\nPonto com esse index não existe. Ind:"  ++ (show index) ++ "\n Pontos" ++ (show pontos)
            else do
                let p2 = pontoMaisProximo pontos (head p1)
                let arvore = [((head p1),p2)]
                arvBuild pontos arvore

--cresce a arvore apos iniciada
arvBuild :: [Ponto] -> Arvore -> Arvore
arvBuild [] arvore = arvore -- sem pontos para adicionar
arvBuild _ [] = error "arvBuild\n Arvore não deve entrar vazia aqui" 
arvBuild pontos arvore = do
    let nos_arv = arvPontos arvore --pontos na arvore
    let lista = difList nos_arv pontos --pontos fora da arvore
    if lista == []
        then arvore -- nao tem mais pontos a adicionar na arvore
    else do
        let dupla = pontoMaisProximoN nos_arv lista 
        let nova_arvore = dupla : arvore --concatena galho a nova arvore
        arvBuild pontos nova_arvore
----------------------
