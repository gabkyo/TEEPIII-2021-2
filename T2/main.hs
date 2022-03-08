import Leitura
import Cidades

main = do
    texto <- readFile cidadesTxt 
    let nomes = leCidades $ lines texto
    texto <- readFile diariasTxt
    let dc = leDiaria $ lines texto
    let tamanho = length nomes
    if tamanho /= (length dc)
        then error ("main\nNumero de cidades e de custos/diarias eh diferente. Cidades faltando ou sobrando\n" ++ (show  nomes) ++ "\n" ++ (show dc))
    else do
        let cidades = listaCidades nomes dc
        print cidades
        print ""
        print $ item3 cidades
        print $ item4 cidades
        print $ item5 cidades
