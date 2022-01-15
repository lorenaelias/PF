-- FUNCOES LAMBDA
-- CURRIED
-- FUNCOES DE VARIOS PARAMETROS SAO TRADUZIDAS EM FUNCOES DE UNICO PARAMETRO
-- A menos que seja explicitamente necessário o uso de tuplas, todas as
-- funções em Haskell são normalmente definidas em forma de curried

-- composicao de funcoes

f x = ceiling (negate (tan (cos (max 50 x))))
--        ==
--fn = ceiling . negate . tan . cos . max 50

g = \x -> x+1

-- funcao anonima calcula a soma de 3 argumentos
h = \a b c -> a+b+c

-- soma de uma par
somaPar = \(x,y) -> x+y

-- calcular fatorial
fatorial = \n -> product [1..n]

-- calcular o imc
imc = \(nome,peso,altura) -> (peso/(altura^2))

-- multiplos 3
mul3 = \xs -> filter (\x->x `mod` 3 == 0) xs

-- soma usando currying
soma' :: Int -> (Int -> Int)
soma' x = terminaSoma
         where terminaSoma y = x+y

soma'' = \x -> (\y -> x+y)

ex1 :: Char -> String -> String
ex1 = \c -> (\str -> c:str)

ex2 :: String -> Char -> String
ex2 = \str -> (\c -> c:str)

ex3 :: Int -> Bool
ex3 = \n -> n==2

ex4 = \c -> [c] ++ "\n"

ex5 = \n -> (n ** 3)

ex6 = \n -> (3 ** n)

--ex7 = \c -> (\r -> elem (c r)) // ta dando pau como que faz isso disgraça

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

mul = \x -> (\y -> (\z -> x*y*z))