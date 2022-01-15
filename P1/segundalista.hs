--1 lista de elementos contidos no intervalo entre a e b
intervalo :: Int -> Int -> [Int]
intervalo a b |(a == b) = [a]
              |(a > b) = []
              |otherwise = [a..b]

--2 devolve lista com objetos repetidos
listarepetida :: [Int] -> [Int]
listarepetida [] = []
listarepetida (x:xs) = [x] ++ x:listarepetida xs

--3 retorna lista com todos os divisores de n
divisores :: Int -> [Int]
divisores n = [y | y<-[1..n], mod n y == 0]

--4 ordenar lista de inteiros em ordem crescente
ordena :: [Int] -> [Int]
ordena [] = []
ordena (x:xs) = insertionSort x (ordena xs)

--5 ordenar lista de n-uplas segundo o primeiro campo (chave)
ordenaTuplas :: [(Int,t)] -> [(Int,t)]
ordenaTuplas [] = []
ordenaTuplas ((x,c):xs) = insertionTuplas (x,c) (ordenaTuplas xs)

--6 separar uma lista em uma lista que contem uma lista de impares e outra de pares
separaLista :: [Int] -> [[Int]]
separaLista l = [[ x | x <- l, even x],[y | y <- l, odd y]]

--7 retorna uniao(com repeticao) de duas listas dadas
uniaoRepetido :: [Int] -> [Int] -> [Int]
uniaoRepetido l1 l2 = ordena (l1++l2)

--8 receber lista de objetos e um valor inteiro n e retornar todas combinacoes deles com repeticao de tamanho n
todasCombs l n = [ x | x <- t ]
                 where t = mapM (const l) [1..n]

--9 insertionSort
insertionSort :: Int -> [Int] -> [Int]
insertionSort a [] = [a]
insertionSort a (x:xs)
                        |(a < x) = a:(x:xs)
                        |otherwise = x:insertionSort a xs

--insertionTuplas
insertionTuplas :: (Int,t) -> [(Int,t)] -> [(Int,t)] 
insertionTuplas (a,t) [] = [(a,t)]
insertionTuplas (a,t) ((x,c):xs)
                            |(a < x) = (a,t):((x,c):xs)
                            |otherwise = (x,c):insertionTuplas (a,t) xs

--quickSort
qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) = qSort esq_x ++ [x] ++ qSort dir_x
               where
                    esq_x = [y | y <- xs, y < x]
                    dir_x = [y | y <- xs, y >= x]

pertence :: Int -> [Int] -> Bool
pertence x [] = False
pertence e (x:xs)
                |(e==x) = True
                |otherwise = pertence e xs

--10 remover repeticao de uma lista
removeRepeticao :: [Int] -> [Int]
removeRepeticao [] = []
removeRepeticao l = uniao [] l

--11a recebe duas listas e retorna o conjunto uniao
ex11 :: [Int] -> [Int] -> [Int]
ex11 l1 l2 = removeRepeticao (auxUniao l1 l2)

--11b
auxUniao :: [Int] -> [Int] -> [Int]
auxUniao l [] = l
auxUniao l (x:xs) =
    if (pertence x l == True) then auxUniao l xs else auxUniao (x:l) xs

uniao :: [Int] -> [Int] -> [Int]
uniao l1 l2 = ordena (auxUniao l1 l2)

--11c
uniaoporCompreensao :: [Int] -> [Int] -> [Int]
uniaoporCompreensao l1 l2 = [x|x <- l1, pertence x l1 == False]++[y | y <- l2, pertence y l2 == False]

--12a multiplos de 3
mul3 :: [Int]
mul3 = [x|x <- [100..300], x `mod` 3 == 0]

--12b numeros primos
listaDiv :: Int -> [Int]
listaDiv n = [p | p<-[2..(n-1)], (n `mod` p == 0)]

verificaPrimo :: Int -> Int -> Bool
verificaPrimo n 1 = True
verificaPrimo n x
    |(n `mod` x == 0) = False
    |otherwise = verificaPrimo n (x-1)

primo :: Int -> Bool
primo n = verificaPrimo n (n-1)

listaprimos :: [Int]
listaprimos = [2] ++ [x | x <- [3..1000], (primo x) == True]

--12c lista de pares (a,b) onde a está em l1 e b está em l2
listaPares :: [Int] -> [Int] -> [(Int,Int)]
listaPares l1 l2 = [ (x,y) | x<-l1, y<-l2 ]

--12d listar de todas as frações possíveis com numeradores de l1 e denominadores de l2
fraction :: [Int] -> [Int] -> [(Int,Int)]
fraction l1 l2 = [(x,y) | x <- l1, y <- l2, y /= 0]

--12e listar valores do polinomio x^2 + 7*x + 1 em [0.0,0.01,0.02..1.0]
listaPol :: [Float]
listaPol = [0.0,0.01..1.00]

pol :: [Float] -> [Float]
pol [] = []
pol (x:xs) = ((x*x)+(7*x)+1):pol xs

polAux :: [Float] -> [Float]
polAux listaPol = pol listaPol

polPrincipal :: [Float]
polPrincipal = polAux listaPol

--12f conjunto de todos os valores que podem ser assumidos por um conjunto de 4 bits
listaBits :: [(Int,Int,Int,Int)]
listaBits = [ (a,b,c,d) | a <- [0,1], b <- [0,1], c <- [0,1], d <- [0,1]]

-- remover todos os elementos n de uma lista
removeTodos :: Int -> [Int] -> [Int]
removeTodos n [] = []
removeTodos n (x:xs)
                    | x == n = removeTodos n xs
                    | otherwise = x:removeTodos n xs

-- remover todos os elementos n com compreensao de lista
removeTodosComp :: Int -> [Int] -> [Int]
removeTodosComp n l = [ x | x <- l, n /= x]

-- usando função de alta ordem 'filter'
removeTodosA :: Int -> [Int] -> [Int]
removeTodosA n l = filter (/=n) l