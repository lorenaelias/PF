--geração de valores aleatórios:
semente :: Integer 
semente = 17489
multiplicador :: Integer
multiplicador = 25173
incremento :: Integer
incremento = 13849
modulo :: Integer
modulo = 65536

proxNumAleat :: Integer -> Integer
proxNumAleat n = (multiplicador*n + incremento) `rem` modulo

seqAleatoria :: (Integer -> [Integer])
seqAleatoria semente = iterate (proxNumAleat) semente

dist :: Num t => [(t, Float)]                      
dist = [(1,0.2),(2,0.25),(3,0.25),(4,0.15),(5,0.1),(6,0.05)]
                     
escalaSequencia :: Integer-> Integer-> ([Integer] -> [Integer])
escalaSequencia a b = map  scala 
 where
     scala n = (div n denom) + a
     faixa = b - a + 1 
     denom = div modulo faixa

geraFuncao :: [(t,Float)] -> (Float -> t)
geraFuncao dist = geraFun dist 0.0
geraFun ((ob,p):dist) nUlt aleat
 | nProx >= aleat && aleat > nUlt = ob
 |otherwise = geraFun dist nProx aleat
   where nProx = (p* (fromInteger modulo) + nUlt)