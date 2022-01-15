import System.IO

escreveDados :: String -> IO ()
escreveDados f = 
     do
        putStrLn "Escrevendo em arquivo ..."
        h <- openFile f WriteMode
        escreveDadosEmArquivo 5 h
        hClose h

escreveDadosEmArquivo :: Int -> Handle -> IO ()
escreveDadosEmArquivo n h
  | n == 0 = return ()
  | otherwise =
       do
          putStr "Digite um valor:"
          l <- getLine
          hPutStrLn h l
          escreveDadosEmArquivo (n-1) h

leDados :: String -> IO ()
leDados f = 
     do
        h <- openFile f ReadMode
        l <- leDadosDeArquivo h
        putStr "soma: "
        putStrLn(show (sum l))
        hClose h

leDadosDeArquivo :: Handle -> IO ([Int])
leDadosDeArquivo h =
   do
     x <- hIsEOF h
     if x
     then return ([])
     else do y <- hGetLine h
             resto <- leDadosDeArquivo h
             return ((read y):resto)


somaLista :: (Num x) => [x] -> x
somaLista [x] = x
somaLista (x:xs) = x+(somaLista xs)