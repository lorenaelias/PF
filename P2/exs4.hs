-- Polimorfismo

-- parametrico
tamanhoLista :: [a] -> Int -- serve para qualquer tipo de lista
tamanhoLista [] = 0
tamanhoLista (x:xs) = 1 + tamanhoLista xs

-- sobrecarga (ad-hoc)
--(+) :: Num a => a -> a -> a
-- le-se: para todo tipo 'a'que pertence a classe dos numeros tem-se o tipo a -> a -> a

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
--elem' x (y:ys) = (x == y) || elem
