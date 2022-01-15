import Data.Char hiding(isDigit)
import Prelude hiding(isDigit)
import GHC.Unicode hiding(isDigit)

--1 calcula o triplo
triplo :: Int -> Int
triplo x = 3*x

-- calcula maior de 2 numeros dados
maiorde2 :: Int -> Int -> Int
maiorde2 x y =
    if x > y then x else y

--2 calcula maior de 3 numeros dados
maiorde3 :: Int -> Int -> Int -> Int
maiorde3 x y z
    |(x >= maiorde2 y z) = x
    |(y >= maiorde2 x z) = y
    |otherwise = z

maiorde3semguarda :: Int -> Int -> Int -> Int
maiorde3semguarda x y z = maiorde2 (maiorde2 x y) z

--3 somatorio de 1 a n
somatorio :: Int -> Int
somatorio 1 = 1
somatorio n = n+somatorio(n-1)

--4 calcula valor do n-esimo termo da PG
nesimoPA :: Int -> Int -> Int -> Int
nesimoPA a1 r 1 = a1
nesimoPA a1 r n = nesimoPA (a1+r) r (n-1)

--5 calcula n-esimo termo da PG
nesimoPG :: Int -> Int -> Int -> Int
nesimoPG a1 q 1 = a1
nesimoPG a1 q n = nesimoPG (a1*q) q (n-1)

--6 somatorio n termos da PA
somaPA :: Int -> Int -> Int -> Int
somaPA 1 r an = an
somaPA n r an = an + (somaPA (n-1) r (an-r))

--7 somatorio n termos PG
somaPG :: Int -> Int -> Int -> Int
somaPG 1 q a1 = a1
somaPG n q a1 = (somaPG (n-1) q (a1*q)) + a1

--8  n-esimo termo de uma sequencia de fibonacci dois primeiros termos 1 e 1
nesimoFib :: Int -> Int
nesimoFib 1 = 1
nesimoFib 2 = 1
nesimoFib n    |n>0 = nesimoFib (n-1) + nesimoFib (n-2)
               |otherwise = error "Insira um numero positivo"

--9 verificar se um ano é bissexto
anoBissexto :: Int -> Bool
anoBissexto n
               |(n < 0) = error "Um ano é um inteiro positivo"
               |((n `mod` 4 == 0) && (n `mod` 100 /= 0)) || (n `mod` 400 == 0) = True
               |otherwise = False

--gera lista de divisores impares positivos de n
listaDiv :: Int -> [Int]
listaDiv n = [p | p<-[2..n], (n `mod` p == 0)]

--10 verificar se um p é primo
verificaPrimo :: Int -> Bool
verificaPrimo n
               |(n <= 0) = error "A definição de primos não se aplica a negativos"
               |(n `mod` 2 /= 0) && (listaDiv n == [n]) = True
               |otherwise = False

--Converte char em int
convertChar :: Char -> Int
convertChar x = ord x

--11 testar se c é maiusculo
maiusculo :: Char -> Bool
maiusculo c
            |(convertChar c > 64) && (convertChar c < 91) = True
            |otherwise = False

--12 testar se c é minusculo
minusculo :: Char -> Bool
minusculo c
            |(convertChar c > 96) && (convertChar c < 123) = True
            |otherwise = False

--13 verificar se c é um digito
isDigit :: Char -> Bool
isDigit c = 
    if((convertChar c > 47) && (convertChar c < 58)) then True else False

--14 repetir n vezes uma string
repete :: String -> Int -> String
repete str 0 = ""
repete str n = str ++ repete str (n-1)

--15 repetir n vezes ' '
replicaEspaco :: Int -> String
replicaEspaco 1 = " "
replicaEspaco n = " " ++ replicaEspaco (n-1)

--menor de dois elementos
menorde2 :: Int -> Int -> Int
menorde2 x y =
    if x < y then x else y

--menor de tres elementos
menorde3 :: Int -> Int -> Int -> Int
menorde3 x y z = menorde2 (menorde2 x y) z

--16 par formado pelo menor e maior elemento (menor,maior)
menorMaior :: Int -> Int -> Int -> (Int,Int)
menorMaior a b c = (menorde3 a b c,maiorde3 a b c)

--calcular o mdc de dois inteiros
mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b
         |(a `mod` b == 0) = b
         |(b `mod` a == 0) = a
         |a>b = mdc b (a `mod` b)
         |a<b = mdc a (b `mod` a)

--mdc usando compreensao de lista
divisoresComum :: Int -> Int -> [Int]
divisoresComum a b = [x| x <- [1..a], a `mod` x == 0, b `mod` x == 0]

mdcmax a b = maximum(divisoresComum a b) 

--17 mdc de euclides
euclidesMdc :: Int -> Int -> Int
euclidesMdc a b
         |(a == b) = b
         |a > 0 = mdc (b `mod` a) a

--18 retornar tupla com 3 elementos em ordem crescente
ordemCrescente :: Int -> Int -> Int -> (Int, Int, Int)
ordemCrescente a b c
 |(maiorde3 a b c == a) && b>c = (c,b,a)
 |(maiorde3 a b c == a) && c>b = (b,c,a)
 |(maiorde3 a b c == b) && a>=c = (c,a,b)
 |(maiorde3 a b c == b) && c>=a = (a,c,b)
 |(maiorde3 a b c == c) && a>b = (b,a,c)
 |otherwise = (a,b,c)

--19 dados dois pontos, retorna a reta formada por eles
type Ponto = (Double,Double)
type Reta = (Double,Double)

reta :: Ponto -> Ponto -> Reta
reta (a1,b1) (a2,b2) = (coef,b1)
                             where y = (b2-b1)
                                   x = (a2-a1)
                                   coef = y / x

type Reta1 = (Ponto,Ponto)

--20 verificar se uma reta dada é vertical
retaVertical :: Reta1 -> Bool
retaVertical ((a,b),(c,d)) = if a == c then True else False

--verificar se uma reta dada é horizontal
retaHorizontal :: Reta1 -> Bool
retaHorizontal ((a,b),(c,d)) = if b == d then True else False

--21 dadas duas retas concorrentes retorne um ponto de intersec
pontoIntersec :: Reta -> Reta -> Double
pontoIntersec (a,b) (c,d) = 
    if (a >= c) then ((d-b)/(a-c)) else ((b-d)/(c-a))

--22 dada uma ordenada retorna o ponto da reta que tem aquela ordenada
pontoOrdenada :: Double -> Reta -> Ponto
pontoOrdenada y (a,b) = (z,y)
                        where z = (y / a) + b