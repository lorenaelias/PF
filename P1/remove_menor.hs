--retorna o menor valor de uma lista
pegar_menor :: [Int] -> Int
pegar_menor [x] = x
pegar_menor (x:xs) | (x < pegar_menor xs) = x
                   | otherwise = pegar_menor xs

--retorna uma lista retirado o menor valor
remove_menor :: [Int] -> [Int]
remove_menor [] = []
remove_menor (x:xs) | (x == pegar_menor (x:xs)) = xs
                    | otherwise = (x:remove_menor xs)

--retorna uma lista ordenada
aux_ordena :: [Int] -> [Int] -> [Int]
aux_ordena lista_ordenada [] = lista_ordenada
aux_ordena lista_ordenada (x:xs) = 
  aux_ordena (lista_ordenada++[pegar_menor (x:xs)]) (remove_menor (x:xs))

ordena :: [Int] -> [Int]
ordena [] = []
ordena lista = aux_ordena [] lista