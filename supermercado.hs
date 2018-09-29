------  Lorena Elias 11721BCC019 - UFU  -------
--  Emissão de Recibo Fiscal de um Supermercado

type Nome = String
type Preco = Int
type CodBar = Int
type BaseDeDados = [(CodBar,Nome,Preco)]
type ListaDeCodigos = [CodBar]
type Recibo = [(Nome,Preco)]


listaDeProdutos :: BaseDeDados
listaDeProdutos = [ (1234, "Oleo DoBom, 1l", 195),
                    (4756, "Chocolate Cazzeiro, 250g", 180),
                    (3216, "Arroz DoBom, 5Kg", 213),
                    (5823, "Balas Pedregulho, 1Kg", 379),
                    (4719, "Queijo Mineirim, 1Kg", 449),
                    (6832, "Iogurte Maravilha, 1Kg", 499),
                    (1112, "Rapadura QuebraDente, 1Kg", 80),
                    (1111, "Sal Donorte, 1Kg", 221),
                    (1113, "Cafe DoBom, 1Kg", 285),
                    (1115, "Biscoito Bibi, 1Kg", 80),
                    (3814, "Sorvete QGelo, 1l", 695)] 

tamLinha :: Int
tamLinha = 30

formataCentavos :: Preco -> String
formataCentavos p 
                  |qtd_cents < 10 = show(div p 100) ++ ".0" ++ show qtd_cents
                  |otherwise = show(div p 100) ++ "." ++ show qtd_cents
                  where
                    qtd_cents = rem p 100

replicar :: Char -> Int -> String
replicar x 1 = [x]
replicar x n = x:(replicar x (n-1))

formataLinha :: (Nome,Preco) -> String
formataLinha (a,b) = 
  a ++ (replicar '.' (tamLinha - (length a + length (formataCentavos b)))) ++ formataCentavos b

formataLinhas :: [(Nome,Preco)] -> String
formataLinhas [] = []
formataLinhas (x:xs) = 
  formataLinha x ++ "\n" ++ formataLinhas xs

geraTotal :: Recibo -> Preco
geraTotal [] = 0
geraTotal ((a,b):xs) = b + geraTotal xs

formataTotal :: Preco -> String
formataTotal p ="Total" ++ replicar '.' (tamLinha-(length "Total" + length (formataCentavos p) + 1)) ++ "$" ++ formataCentavos p 

formataRecibo :: Recibo -> String
formataRecibo p = printaNome ++ formataLinhas p ++ formataTotal (geraTotal p)

printaNome :: String
printaNome = "SupermercadoQLegal\n"

acha :: BaseDeDados -> CodBar -> (Nome,Preco)
acha [] _ = ("item desconhecido",0)
acha ( (codi,prod,prec):xs) cod  
   |cod == codi = (prod,prec)
   |otherwise = acha xs cod

achaItem :: CodBar -> (Nome,Preco)
achaItem cod = acha listaDeProdutos cod

fazRecibo :: ListaDeCodigos -> Recibo
fazRecibo [] = []
fazRecibo (x:xs) = achaItem x : fazRecibo xs

geraRecibo :: ListaDeCodigos -> String
geraRecibo lc = formataRecibo( fazRecibo lc)