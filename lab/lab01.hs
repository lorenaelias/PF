-- Soma de dois elementos
-- parametro1 -> parametro2 -> saida
-- soma :: double -> double -> double
soma a b = a + b -- função com parametros a e b

-- quadrado :: double -> double
quadrado a = a * a

-- double x = x + x

-- quadruple x = double (double x)

-- misterio x y z w = soma (soma x y) (soma z w)

-- hipotenusa a b = sqrt((quadrado a) + (quadrado b))
-- hipotenusa :: double -> double -> double
hipotenusa a b = sqrt(soma((quadrado a) (quadrado b)))