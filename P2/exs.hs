import Data.Char

-- n = a `div` length xs
--     where
--          a = 10
--          xs = [1,2,3,4,5]

-- last' [] = error "nao existe ultimo elemento"
-- last' [a] = a
-- last' (x:xs) = last' xs

-- removeultimo [] = []
-- removeultimo [a] = []
-- removeultimo (x:xs) = x:removeultimo xs

-- removeElem :: Int -> [Int] -> [Int]
-- removeElem a [] = []
-- removeElem a (x:xs) = if (a==x) then xs else x: removeElem a xs

f a b = let y = a*b
            g x = (x+y)/y
    in g (2*a)+g (3*b)

pegaLetras [] = []
pegaLetras (x:xs) | isAlpha x = x:pegaLetras xs
                  | otherwise = pegaLetras xs

-- tentar implementar
-- "aaabbc"[(a,3),(b,2),(c,1)]