-- Compreensão de Listas 

letras :: [Char]        -- funções devem ter primeira letra minuscula!
letras = ['a','b','c']

--  [<Limite inferior> .. <Limite superior>]
--  [1..4] gera [1,2,3,4]
--  [1,3..6] gera [1,3,5]
--  ['a','d'..'p'] gera ["adgjmp"]
--  [3.1..7] gera [3.1,4.1,5.1,6.1,7.1]
--  [5,6..5] gera [5]

listaQuad = [ x^2 | x <- [1..30]]
listaQuadInfinito = [ x^2 | x <- [1..]]

-- numero*2 em cada posicao da lista em que x>0
dobraPos::[Int] -> [Int]
dobraPos xs = [2*x | x <- xs, x>0]

dobraPosRec :: [Int] -> [Int]
dobraPosRec [] = []
dobraPosRec (x:xs)          -- antes da guarda nao tem sinal '='
                   | x>0 = (2*x:dobraPosRec xs)
                   | otherwise = dobraPosRec xs

-- concatenaListas :: [t] -> [t] -> [t]
-- concatenaListas [] [] = []
-- concatenaListas l [] = l
-- concatenaListas (x:xs) lista = (x:(concatenaListas (xs:lista)))

fat :: Int -> Int
fat 0 = 1
fat n = n*(fat (n-1))