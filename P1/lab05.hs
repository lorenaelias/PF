-- dobrar os elementos de uma lista de inteiros
-- dobra :: [Int] -> [Int]
-- dobra [] = []
-- dobra (x:xs) = 2*x:(dobra xs)  

 --ou
dobra :: [Int] -> [Int]
dobra l = [2*y | y <- l]
 
  -- obter todos os elementos pares de uma lista
 --   dobra_pares :: [Int] -> [Int]
 --   dobra_pares [] = []
 --   dobra_pares (x:xs)
 --     |even x = x:(dobra_pares xs)
 --     |otherwise = dobra_pares xs
 
 --ou
 --dobra_pares :: [Int] -> [Int]
 --dobra_pares l = [(2*y) | (y <- l), (even y)]
 
 -- concatena strings
 concatStr :: [String] -> String
 concatStr [] = []
 concatStr (x:xs) = x ++ (concatStr xs)

 -- fold : combinacao de uma funcao com uma estrutura de dados(lista)
-- fold (+) [1,2,3,4,5] gera 15 = 1+2+3+4+5
