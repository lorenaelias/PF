------------------------------------{- Trabalhando com Simulações em Haskell -}------------------------------------
------------------------------------{-               UFU 2018-2              -}------------------------------------
------------------------------------{-      Lorena Elias - 11721BCC019       -}------------------------------------


--4.
-- estender o EstadoDoServidor com uma fila extra que alimenta as filas individuais: 
-- um elemento deixa a fila do alimentador quando uma das filas pequenas está vazia.


-- tipo de dado de um cliente que chega
data ClienteQChega = Nao | Sim TempoQChegou TempoPAtend deriving (Show,Eq,Ord,Read)
type TempoQChegou = Int
type TempoPAtend = Int

-- tipo da mensagem de saída do cliente
data ClienteQSai = Nenhum |Liberado TempoQChegou TempoDEsp TempoPAtend deriving (Show,Eq,Ord,Read)
type TempoDEsp = Int

------------------------------------ Processamento do estado da fila -----------------------------------

-- Simula o processo de enfileiramento
type EstadoDaFila = (Tempo, TempoDeAtend,[ClienteQChega])
type Tempo = Int
type TempoDeAtend = Int

-- adiciona um cliente que deseja entrar no fim da fila
adicionaCliente :: ClienteQChega -> EstadoDaFila -> EstadoDaFila
adicionaCliente c1 (t,tAtend,f) = (t,tAtend,f++[c1])

-- trata o elemento da cabeça da fila até que ele seja atendido e, logo após, retira esse elemento dela
processaFila :: EstadoDaFila -> (EstadoDaFila, [ClienteQSai])
processaFila (tempo,tempDeAtend,[]) = ((tempo+1,tempDeAtend,[]),[])  
processaFila (tempo,tempDeAtend,(Sim a tempNecDAtend:resto))
    |tempDeAtend < tempNecDAtend =(((tempo+1),tempDeAtend+1,(Sim a tempNecDAtend:resto)),[])  -- nenhum cliente é liberado
    |otherwise = ((tempo+1,0,resto),[Liberado a (tempo-tempNecDAtend-a) tempNecDAtend])       -- cliente é liberado

-- devolve o numero de elementos que ainda estao na fila
tamanhoDaFila :: EstadoDaFila -> Int
tamanhoDaFila (_,_,l) = length l

-- testa se tem algum elemento na fila
filaVazia :: EstadoDaFila -> Bool
filaVazia (t,s,q) = (q == [])

filaDeInicio :: EstadoDaFila
filaDeInicio = (0,0,[])

-------------------------------- Processamento do estado do servidor --------------------------------

-- o novo tipo do EstadoDoServidor contem uma fila que alimenta as outras
type EstadoDoServidor = ([EstadoDaFila],[ClienteQChega])

-- avalia cada fila que está no servidor e concatena as saídas produzidas por elas
processaServidor :: EstadoDoServidor -> (EstadoDoServidor,[ClienteQSai])
processaServidor ([],l) = (([],l),[])
processaServidor (filas,l) = ((lista,l),saida)
                    where
                        (lista,saida) = processaServidorAux filas
processaServidorAux :: [EstadoDaFila] -> ([EstadoDaFila], [ClienteQSai])
processaServidorAux [] = ([],[])
processaServidorAux (q:qs) = ((nq:nqs), mess ++ messes)
                    where
                       (nq,mess) = processaFila q
                       (nqs,messes) = processaServidorAux qs

-- processa o servidor e caso chegue um cliente, o adiciona numa fila vazia
-- retorna o estado após o minuto processado
processaSimulacao :: EstadoDoServidor -> (EstadoDoServidor,[ClienteQSai])
processaSimulacao estServ = (adicionaNovoObjeto estServ1,clientQSai)
                       where
                          (estServ1,clientQSai) = processaServidor estServ

adicionaNovoObjeto :: EstadoDoServidor -> EstadoDoServidor
adicionaNovoObjeto (((t,tAt,l):xs),[]) = (((t,tAt,l):xs),[])
adicionaNovoObjeto (lista,(y:ys)) = 
    if ((existe lista)==True) 
        then (buscaVazia lista y , ys) 
        else (lista,(y:ys))

-- funcoes auxiliares --
existe :: [EstadoDaFila] -> Bool
existe [] = False
existe ((t1,t2,[]):xs )  = True
existe ((t1,t2,l):xs) = existe xs

buscaVazia :: [EstadoDaFila] -> ClienteQChega -> [EstadoDaFila]
buscaVazia ((t,tAt,[]):xs) y = ((t,tAt,[y]):xs)  
buscaVazia ((t,tAt,l):xs ) y = ((t,tAt,l):(buscaVazia xs y))

-- adiciona ao servidor a quantidade de filas especificada em estadoInicialDoServidor
copy :: Int -> EstadoDaFila -> [EstadoDaFila]
copy nroDeFilas estServ
    |nroDeFilas == 0 = []
    |otherwise = [estServ] ++ (copy (nroDeFilas-1) estServ)

-- inicializa o servidor com um número dado de filas
estadoInicialDoServidor :: EstadoDoServidor
estadoInicialDoServidor = (copy nroDeFilas filaDeInicio, take nPessoas entradaDaSimulacao2)

--------------------------------- Geração de valores pseudo-aleatórios --------------------------------

semente :: Integer
semente = 17489
multiplicador :: Integer
multiplicador = 25173
incremento :: Integer
incremento = 13849
modulo :: Integer
modulo = 65536

-- congruências lineares, usada para geração de números aleatórios
proxNumAleat :: Integer -> Integer 
proxNumAleat n = rem (multiplicador*n + incremento) modulo

-- gera uma sequência de números aleatórios
seqAleatoria :: (Integer -> [Integer])  
seqAleatoria semente = iterate proxNumAleat semente

-- R. os parenteses do parametro da funcao acima, servem para denotar que será aplicada uma função no argumento dentro dele
--    portanto, retirá-los mudaria o intuito da função

-- distâncias que serao usadas na geraFuncao
dist :: Num t => [(t, Float)]
dist = [(1,0.2), (2, 0.25), (3, 0.25), (4, 0.15), (5,0.1), (6,0.05)]

-- função que transforma uma distribuição de probabilidades de tempo de espera de cada cliente numa lista infinita
geraFuncao :: [(t,Float)] -> (Float -> t)
geraFuncao dist = geraFun dist 0.0
geraFun ((ob,p):dist) nUlt aleat | nProx >= aleat && aleat > nUlt = ob
                                 | otherwise = geraFun dist nProx aleat
                                 where nProx = (p* ( fromInteger modulo)) + nUlt

-------------------------------------- Simulação do Servidor ---------------------------------------
simule :: EstadoDoServidor -> [ClienteQSai]
simule (filas,[]) = []
simule estDoServ = outmesses ++ simule proxEstDoServ 
                        where (proxEstDoServ,outmesses) = processaSimulacao estDoServ 

-- gera uma sequência de números aleatórios de tempos de atendimento para entrada na simulação
seqDeTempos :: [TempoPAtend]
seqDeTempos = map ((geraFuncao dist).fromInteger) (seqAleatoria semente)

-- simulação para uma entrada infinita
entradaDaSimulacao = zipWith Sim [ 1..] seqDeTempos

-- simulação para uma entrada limitada
entradaDaSimulacao2 = take nPessoas entradaDaSimulacao ++ naos 
    where 
            naos = (Nao:naos)

-- Tempo total de espera para uma sequência finita de ClienteQSai
tempoDeEsperaTotal :: ([ClienteQSai] -> Int)
tempoDeEsperaTotal = sum . map tempoDEsp
    where 
            tempoDEsp (Liberado _ w _) = w

-- as constantes abaixo serão usadas para definir o número de filas e o número de pessoas desejados
nroDeFilas :: Int
nroDeFilas = 1

nPessoas :: Int
nPessoas = 50

-- foi preciso modificar as funçoes das seções de "Processamento do Estado do Servidor" 
-- e "Simulação do Servidor" para se adequarem à nova especificacao de fila única

-- usando tempoDeEsperaTotal (simule estadoInicialDoServidor)
-- podemos comparar com as respostas anteriores e concluir que 
-- essa implemetação é mais eficiente do que as outras

-- para 1 fila e 50 pessoas
-- temos um tempo de espera total de 3221 minutos

-- para zerar o tempo de espera total com 50 pessoas sao precisas 4 filas