-- Tipos Enumerados
data Dia = Dom | Seg | Ter | Qua | Qui | Sex | Sab

-- definindo funcoes baseadas em tipos algebricos
finalSemana Sex = True
finalSemana Sab = True
finalSemana _ = False

-- definicao com guardas nao funciona
-- os tipos algebricos criados nao possuem a operacao igualdade reconhecida
fds a
        | (a == Sex) = True
        | (a == Sab) = True
        | otherwise = False

-- para usar igualdade, podemos declara-la como uma instancia de classe Eq e definir a funcao igualdade
instance Eq Dia where 
      (==) Seg Seg = True
      (==) Ter Ter = True
      (==) Qua Qua = True
      (==) Qui Qui = True
      (==) Sex Sex = True
      (==) Sab Sab = True
      (==) Dom Dom = True
      (==) _ _ = False

-- para nao ficar cansativo de se definir sempre essas funcoes basicas pode-se usar

data Est = Primavera | Verao | Outono | Inverno
           deriving Eq

verao a
        | (a == Verao) = True
        | otherwise = False

data Pessoa = Ind String String Int deriving (Show) 
p1,p2,p3::Pessoa
p1 = Ind "Stephen" "Hawking" 1942
p2 = Ind "Albert" "Einstein" 1879
p3 = Ind "Isaac" "Newton" 1643

-- funcoes sobre manipulacao de infos sobre uma pessoa
primeiroNome :: Pessoa -> String
primeiroNome (Ind pNome _ _) = pNome

ultimoNome :: Pessoa -> String
ultimoNome (Ind _ uNome _) = uNome

anoNascimento :: Pessoa -> Int
anoNascimento (Ind _ _ vAno) = vAno

-- ou entao
data Pessoas = Ind2 { primNome :: String, ultNome::String, anoNasc::Int }
              deriving (Show)
p :: Pessoas
p = Ind2 { primNome = "Carlos",ultNome= "Lopes",anoNasc = 1800 }

data Forma = Circulo Float Float Float | Retangulo Float Float Float Float
             deriving (Show)

area :: Forma -> Float
area (Circulo _ _ r) = pi * r ^ 2


data Lista a = Nil | Cons a (Lista a)

conta :: (Lista a) -> Int
conta Nil = 0
conta (Cons a b) = 1 + conta b

quant :: (Lista a) -> Int 
quant Nil = 0
quant (Cons _ b) = 1 + quant b