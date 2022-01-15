module Main (main) where

import System.IO 

main = do 
    putStr "digita um grau fahrenheit ai ae: "
    t <- readLn
    putStrLn (show (celsius t))

celsius :: Double->Double
celsius f = (5.0/9) * (f-32)