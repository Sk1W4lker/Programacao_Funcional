data Hora = H Int Int
            deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

--Auxiliares
--a) testar se um par de inteiros representa uma hora do dia v ́alida;
horaValida :: Hora -> Bool
horaValida (H p1 p2) = p1 >= 0 && p1 <= 23 && p2 >= 0 && p2 <= 59

--b) testar se uma hora  ́e ou n ̃ao depois de outra (compara ̧c ̃ao);
horaTempoDeChegadaSuperior :: Hora -> Hora -> Bool
horaTempoDeChegadaSuperior (H p1 p2) (H p3 p4) = p1 == p2 && p4 > p3 || p3 > p1 

--c) converter um valor em horas (par de inteiros) para minutos (inteiro);
converteHoras :: Hora -> Int
converteHoras (H p1 p2) = p1*60+p2

--d) converter um valor em minutos para horas;
converteMinutosHoras :: Int -> Hora
converteMinutosHoras l = H (div l 60) (mod l 60) --mod é o resto da divisão

--e) calcular a diferenca entre duas horas (cujo resultado deve ser o numero de minutos)
diferencaHoras :: Hora -> Hora -> Int
diferencaHoras (H p1 p2) (H p3 p4) = (p1-p3)*60 + (p2-p4)

--f) adicionar um determinado numero de minutos a uma dada hora
adicionarMinutos :: Hora -> Int -> Hora
adicionarMinutos (H h m) min = converteMinutosHoras (converteHoras (H h m) + min)

--a) Testar se uma etapa est ́a bem constru ́ıda (i.e., o tempo de chegada  ́e superior ao de partida e as horas s ̃ao v ́alidas).
etapaBemConstruida :: Etapa -> Bool
etapaBemConstruida (p1,p2) = horaValida p1 && horaValida p2 && horaTempoDeChegadaSuperior p1 p2 

--b) Testa se uma viagem est ́a bem constru ́ıda (i.e., se para cada etapa, o tempo de chegada  ́e superior ao de partida, e se a etapa seguinte come ̧ca depois da etapa anterior ter terminado)
viagemBemConstruida :: Viagem -> Bool
viagemBemConstruida [] = True
viagemBemConstruida ((h1,h2):t) = etapaBemConstruida (h1,h2) && viagemBemConstruida t && 
    (case t of [] -> True
               (h3,h4):t' -> h3 `horaTempoDeChegadaSuperior` h2)

--c) Calcular a hora de partida e de chegada de uma dada viagem. 
partidaEChegada :: Viagem -> (Hora,Hora)
partidaEChegada n = (fst (head n), snd (last n)) --fst pega o primeiro elemento do par, snd pega o ultimo elemento do par

{-
partidaEChegada :: Viagem -> (Hora,Hora)
partidaEChegada n = (x,y)
    where (x,_) = head n
    where (_,y) = last n
-}

--d) Dada uma viagem v ́alida, calcular o tempo total de viagem efectiva
tempoViagemEfetiva :: Viagem -> Hora
tempoViagemEfetiva [] = H 0 0
tempoViagemEfetiva ((p1, p2):t) = adicionaHoras (converteMinutosHoras (diferencaHoras p2 p1)) (tempoViagemEfetiva t)

adicionaHoras :: Hora -> Hora -> Hora
adicionaHoras (H p1 p2) (H p3 p4) = H (p1 + p2 + (div (p2+p4) 60)) (mod (p2+p4) 60)

--e) Calcular o tempo total de espera.
tempoEspera :: Viagem -> Hora
tempoEspera ((h1,h2):(h3,h4):t) = adicionaHoras (converteMinutosHoras (diferencaHoras h3 h2)) (tempoEspera ((h3,h4):t))
tempoEspera _ = (H 0 0)

--f) Calcula o tempo total da viagem
tempoTotalViagem' :: Viagem -> Hora
tempoTotalViagem' v = converteMinutosHoras (diferencaHoras (snd (last v)) (fst (head v))) --O ultimo elemento do ultimo par - o primeiro elemento do primeiro par

{-
tempoTotalViagem :: Viagem -> Hora
tempoTotalViagem v = adicionaHoras (tempoViagemEfetiva v) (tempoEspera v)

tempoTotalViagem' :: Viagem -> Hora
tempoTotalViagem' v = diferencaHoras hf hi
    where (hi,hf) = partidaEChegada v
-}

