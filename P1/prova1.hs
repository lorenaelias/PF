-- funcao que retorna a metade de cada numero par da lista
halveEvens :: [Int] -> [Int]
halveEvens xs = map (`div` 2) [ x | x <- xs,mod x 2 == 0 ]

-- mesmo proposito, utilizando recursividade
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec [0] = [0]
halveEvensRec (x:xs) = if (x `mod` 2 == 0) then (x `div` 2):halveEvensRec xs else halveEvensRec xs 

-- implementar funcao de ordem superior existente takeWhile
takeWhile' :: (t -> Bool) -> [t] -> [t]
takeWhile' f [a] = []
takeWhile' f (x:xs) =  if (f x == True) then x:takeWhile' f xs else []