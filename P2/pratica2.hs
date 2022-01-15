--1.
deriv :: Fractional a => (a -> a) -> a -> a -> a
deriv f dx = \x -> (f(x + dx) - f(x)) / dx

asd = foldr1 (\x -> \y -> x + y + 7) [1,2,3,4,5]
--5+7+4+7+3+7+2+7+1

--2.

data NomeP = Nome String
             deriving (Show,Ord,Eq)
             -- derivando Ord é possivel fazer operacoes (<) (>)
             -- derivando Eq é possivel fazer operacoes (==) (/=)

data SobreNomeP = SobreNome String
                  deriving (Show,Ord,Eq)

type NomeCompleto = (NomeP,SobreNomeP)

compara :: NomeCompleto -> NomeCompleto -> Bool
compara (Nome n1,SobreNome s1) (Nome n2,SobreNome s2) | (n1 == n2) && (s1 == s2) = True
                                                      | otherwise = False

--3.                                                    
data Exp a = Val a -- um numero
            | Neg (Exp a)
            | Add (Exp a) (Exp a) -- soma de duas expressoes
            | Sub (Exp a) (Exp a) -- subtracao
            | Mul (Exp a) (Exp a) -- multiplicacao
            | Div (Exp a) (Exp a) -- divisao

avalia :: Fractional a => Exp a -> a
avalia (Val x) = x                                     -- > avalia (Val 1) = 1.0
avalia (Neg exp) = - (avalia exp)                      -- > avalia ( Neg (Val 1) ) = -1.0
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2) -- > avalia (Add (Val 3.154) (Val 2.00)) = 5.154
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2) -- > avalia (Sub (Val 3.154) (Val 2.00)) = 1.154 
avalia (Mul exp1 exp2) = (avalia exp1) * (avalia exp2) -- > avalia (Mul (Val 3.3) (Val 2)) = 6.6  
avalia (Div exp1 exp2) = (avalia exp1) / (avalia exp2) -- > avalia (Div (Val 4) (Val 2)) = 2.0

--4.
data LL = Latitude Int Int Int | Longitude Int Int Int 
          deriving (Eq,Ord)

instance Show LL where
 show (Latitude a b c) = "Lat " ++ show a ++"°" ++ show b ++"'"++ show c ++"''"
-- uma instancia da classe show é um subconjunto dela
-- conseguimos utilizar o que ela oferece para deixar tudo do nosso jeito

type PosicaoLocal = (String, LL, LL)
type Cidades = [PosicaoLocal]

c1,c2::PosicaoLocal
c1 = ("Brasilia", Latitude (-15) 46 47, Longitude 47 55 47)
c2 = ("Uberlandia", Latitude (-18) 55 07, Longitude 48 16 38)

eLat::PosicaoLocal->(String,LL)
eLat (p,(Latitude a b c), (Longitude x y z)) = (p,(Latitude a b c))

norteDe:: PosicaoLocal -> PosicaoLocal -> Bool
norteDe (_,(Latitude a b c),(Longitude _ _ _)) (_,(Latitude d e f),(Longitude _ _ _)) =
    if (g1 > g2) then True else False
    where g1 = a*3600 + b*60 + c
          g2 = d*3600 + e*60 + c

lcidades :: Cidades
lcidades =
  [("Rio Branco", Latitude 09 58 29, Longitude 67 48 36),
   ("Brasilia", Latitude (-15) 46 47, Longitude 47 55 47),
   ("Torres", Latitude (-29) 20 07, Longitude 49 43 37),
   ("Joao Pessoa", Latitude (-07) 06 54, Longitude 34 51 47),
   ("Uberlandia", Latitude (-18) 55 07, Longitude 48 16 38)  ]

quantAbaixoEquador :: Cidades -> Int
quantAbaixoEquador [] = 0
quantAbaixoEquador ((_, Latitude a _ _, Longitude _ _ _):xs) = 
    if (a < 0) then 1 + quantAbaixoEquador xs else quantAbaixoEquador xs

cidadesEntre :: Cidades -> [String]
cidadesEntre [] = []
cidadesEntre ((c, Latitude _ _ _, Longitude a _ _):xs) =
    if ( (a > 40) && (a < 50) ) then [c] ++ (cidadesEntre xs) else cidadesEntre xs

--5.
data Talvez a = Valor a | Nada 
                deriving (Show)

divisaoSegura :: Float -> Float -> Talvez Float
divisaoSegura x y = if (y == 0) then Nada else Valor (x/y)
-- aqui temos um Valor que pode ser 0 ou diferente de 0
-- quando a entrada y é == 0 temos um problema!
-- nao podemos fazer divisao por 0
-- entao com esse tipo de dado podemos simular através de um construtor como no exemplo acima
-- uma resposta vazia, composta somente pelo construtor (Nada)
--Esse encapsulamento é muito interessante para condiçoes de exceção

--6.
addPares :: [(Int,Int)] -> [Int]
addPares lista = [ m+n | (m,n) <- lista ]
--6a.
addParesT :: [(Int,Int)] -> [Int]
addParesT lista = [ m+n | (m,n) <- lista, m<n ]

--6b.         
addPares2 :: (Integral a) => [(a,a)] -> [a]
addPares2 = map (\(m,n) -> m + n)

--6c.
addParesT2 :: (Integral a) => [(a,a)] -> [a]
addParesT2 l= map (\(m,n) -> m+n) (filter (\(m,n) -> m<n) l)

--7.
mp f [] ys = []
mp f xs [] = []
mp f (x:xs) (y:ys) = f x y : mp f xs ys
-- ela aplica a funcao na cabeça das duas listas e transforma em uma unica lista
-- por exemplo
-- > mp (+) [1,2,3] [4,5,6,7]
-- > [5,7,9]

--8.
somaQuad = \n -> foldr1  (\x -> \y -> x + y) (map (^2) [1..n])

--9.
somaQuadPos :: [Integer] -> Integer
somaQuadPos = \xs -> foldr1  (\x -> \y -> x + y) (map (^2) (filter (>(-1)) xs))

--10.
fun x = [x]
misterio :: [a] -> [a]
misterio xs = foldr (++) [] (map fun xs)
--usa o foldr para concatenar a primeira lista (vazia) com elementos da lista passada como parametro