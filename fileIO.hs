module FileIO where

import System.IO

-- a <- getLine, getChar etc atribui como string ou char normal,  a=getLine vira IO String
-- io string diferente

--le um caractere do terminal
--getChar

--le uma linha do terminal
-- getLine

--bota string no terminal
-- putStr
-- putStrLn adiciona \n no finalx

{-
ler linha do arquivo
linha = do
    x <- openFile "nome.txt" ReadMode
    y <- hGetLine x
    putStr y

argumentos dentro de do tem um tab do comeco
-}

printLinhaArq :: String -> IO ()
printLinhaArq nome = do 
    x <- openFile nome ReadMode 
    y <- hGetLine x
    putStr y

nome = "iris.csv"

printArquivo ::  String -> IO String
printArquivo nome = do
    arquivo <- readFile nome
    return arquivo

x = printArquivo nome
