module Ficha01 where
import Data.Char

--1)

--a)
perimetro :: Double -> Double
perimetro l = 2*pi*l

--b)
dist :: (Double, Double) -> (Double, Double) -> Double
dist (x,y) (x2,y2) = sqrt((x-x2)^2 + (y-y2)^2)

--c)
primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

--d)
multiplo :: Int -> Int -> Bool
multiplo x y = mod x y == 0

--e)
truncaImpar :: [a] -> [a]
truncaImpar l = if mod (length l) 2 == 0 then l
            else tail l

--f)
max2 :: Int -> Int -> Int
max2 x y = if x > y then x else y

--g)
max3 :: Int -> Int -> Int -> Int
max3 a b c = max2(max2 a b) c

--2
--a)
nRaizes :: (Double, Double, Double) -> Int 
nRaizes (a,b,c) | b^2 - 4*a*c == 0 = 1
                | b^2 - 4*a*c > 1 = 2
                | b^2 - 4*a*c < 1 = 0
                | otherwise = 0

--b)
raizes :: (Double, Double, Double) -> [Double]
raizes (a,b,c)  | n == 2 = [(-b + sqrt(b ^ 2 - 4 * a * c))/(2*a), (-b-sqrt(b ^ 2 - 4 * a * c))/(2*a)]
                | n == 1 = [-b / (2*a)]
                | n == 0 = []
                where n = nRaizes(a,b,c)

--3)
type Hora = (Int,Int)

--a)
validaHora :: Hora -> Bool
validaHora (h,m) = h >= 0 && h <= 23 && m >= 0 && m <= 59

--b)
horaDepois :: Hora -> Hora -> Bool
horaDepois (h,m) (h2,m2) = h == h2 && m > m2 || h > h2

--c)
converteHoras :: Hora -> Int
converteHoras (h,m) = h*60+m

--d) 
converteMinutosHoras :: Int -> Hora
converteMinutosHoras l = (div l 60, mod l 60)

--e)
diferencaHoras :: Hora -> Hora -> Int
diferencaHoras (h,m) (h2,m2) = (h-h2)*60 + (m-m2)

--f)
adicionaMinutos :: Hora -> Int -> Hora
adicionaMinutos (h,m) n = (h+ hr,mr)
    where (hr,mr) = converteMinutosHoras (m+n)

--4)
data HoraH = H Int Int deriving (Show,Eq)

--a)
horaValida1 :: HoraH -> Bool
horaValida1 (H a b) = a >= 0 && a<=23 && b >=0 && b<=59

--b)
horaDepoisDe1 :: HoraH -> HoraH -> Bool
horaDepoisDe1 (H h1 m1) (H h2 m2) = h1 > h2 || (h1 == h2 && m1 > m2)

--c)
horaParaMinutos1 :: HoraH -> Int
horaParaMinutos1 (H h m) = 60 * h + m

--d)
minutosParaHora1 :: Int -> HoraH
minutosParaHora1 m = (H (div m 60) (mod m 60)) 

--e)
diferencaHoras1 :: HoraH -> HoraH -> Int
diferencaHoras1 (H h1 m1) (H h2 m2) = (h1 - h2) * 60 + (m1 - m2)

--f)
adicionaMinutos1 :: HoraH -> Int -> HoraH
adicionaMinutos1 (H h m) n = H (h + hr) mr
    where (H hr mr) = minutosParaHora1 (m + n)

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

--a)
next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

--b)
stop :: Semaforo -> Bool
stop Vermelho = True
stop _ = False

--c)
safe :: Semaforo -> Semaforo -> Bool
safe Vermelho _ = True
safe _ Vermelho = True
safe _ _ = False

--6)
data Ponto = Cartesiano Double Double | Polar Double Double
            deriving (Show,Eq)

--a)
posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = r * cos a

--b)
posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = r * sin a

--c)
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2 + y^2)
raio (Polar r a) = r

--d) 
angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y/x)
angulo (Polar r a) = a

--e)
dist1 :: Ponto -> Ponto -> Double
dist1 p1 p2 = sqrt ((x1 - x)^2 + (y1-y)^2)
    where x = posx p1
          y = posy p1
          x1 = posx p2
          y1 = posy p2
--7)
data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)
--a)
poligono :: Figura -> Bool
poligono (Circulo _ _) = False --circulos não são poligonos
poligono (Retangulo p1 p2) = posx p1 /= posx p2 && posy p1 /= posy p2
poligono (Triangulo p1 p2 p3) = posx p1 /= posx p2 ||
                                posx p2 /= posx p3 ||
                                posx p1 /= posx p3 
                                &&
                                posy p1 /= posy p2 ||
                                posy p2 /= posy p3 ||
                                posy p1 /= posy p3
--b)
vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Retangulo p1 p2) = [p1, Cartesiano (posx p1) (posy p2), p2, Cartesiano (posx p2) (posy p1)]
vertices (Triangulo p1 p2 p3) = [p1, p2, p3]

--c)
area :: Figura -> Double
area (Circulo _ r) = pi*r^2
area (Retangulo p1 p2) = abs (posx p2 - posx p1) * abs (posy p2 - posy p1)
area (Triangulo p1 p2 p3) = let a = dist1 p1 p2
                                b = dist1 p2 p3
                                c = dist1 p3 p1
                                s = (a+b+c) / 2 -- semi-perimetro
                                in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron

--d)
perimetro1 :: Figura -> Double
perimetro1 (Circulo _ r) = 2*pi*r
perimetro1 (Retangulo p1 p2) = abs(posx p2 - posx p1) * 2 + abs(posy p2 - posy p1) * 2
perimetro1 (Triangulo p1 p2 p3) = dist1 p1 p2 + dist1 p2 p3 + dist1 p1 p3

--8)