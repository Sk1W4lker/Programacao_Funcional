--1
enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 start end 
    | start > end = []
    | otherwise = start : enumFromTo1 (start + 1) end

--2
enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 start next end
    | start > end && next >= start || start < end && next < start = []
    | otherwise = start : enumFromThenTo1 next (next + (next-start)) end

--3
plusPlus :: [a] -> [a] -> [a]
plusPlus [] l = l
plusPlus (h:t) l = h : plusPlus t l

--4
intInt :: [a] -> Int -> a
intInt (h:_) 0 = h
intInt (_:t) l = intInt t (l-1)

--5)
reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (h:t) = t ++ reverse [h]
--ou reverse1 t ++ [h]

--6)
take1 :: Int -> [a] -> [a]
take1 0 (h:t) = t
take1 l (h:t) = h : take1 (l-1) t

--ou
--take _ [] = []
--take n (h:t)
--   | n <= 0 = []
--   | otherwise = h : take (n - 1) t

--7)
drop1 :: Int -> [a] -> [a]
drop1 0 l = l
drop1 l (h:t) = drop1 (l-1) t

--8)
zip1 :: [a] -> [b] -> [(a,b)]
zip1 _ [] = []
zip1 [] _ = []
zip1 (h:t) (h1:t1) = (h,h1) : zip1 t t1 

--9)
replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n x | n < 0 = []
               | otherwise = x : replicate (n - 1) x
--10)
intersperse1 :: a -> [a] -> [a]
intersperse1 l [] = []
intersperse1 l (h:t) | length (h:t) == 1 = [h]
                     | otherwise = h : l : intersperse1 l t
--ou
--intersperse :: a -> [a] -> [a]
--intersperse _ [] = []
--intersperse _ [h] = [h]
--intersperse x (h:t) = h : x : intersperse

--11)
group :: Eq a => [a] -> [[a]]
group [] = []
group [x] = [[x]]
group (h:t) | h `elem` head (group t) = (h : (head (group t))) : tail (group t) 
            | otherwise = [h] : group t

--12)
concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (h:t) = head h : concat1 t

--13)
inits1 :: [a] -> [[a]]
inits1 [] = [[]]
inits1 l = (inits1 (init l)) ++ [l]

{-
inits [11,21,13]
inits [11,21] ++ [11,21,13]
inits [11] ++ [11,21] ++ [11,21,13]
inits [] ++ [11] ++ [11,21] ++ [11,21,13]
[[],[11],[11,21],[11,21,13]]
-}

--14)
tails :: [a] -> [[a]]
tails [] = [[]]
tails (h:t) = (h:t) : tails (tail (h:t)) 

--15)
heads :: [[a]] -> [a]
heads [] = []
heads ([]:t) = heads t
heads (h:t) = (head h) : heads t

--16)
total :: [[a]] -> Int
total [] = 0
total (h:t) = length h + total t

--17)
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = (a,c) : fun t

--18)
cola :: [(String,b,c)] -> String
cola [] = ""
cola ((a,b,c):t) = a ++ cola t

--19)
idade :: Int -> Int -> [(String,Int)] -> [String]
idade a b [] = []
idade ano idade2 ((nome,ano2):t) | (ano - ano2) >= 26 = nome : idade ano idade2 t
                                 | otherwise = idade ano idade2 t

--20)
powerEnumFrom2 :: Int -> Int -> [Int]
powerEnumFrom2 a 1 = [1]
powerEnumFrom2 a b | b <= 0 = []
                   | otherwise = powerEnumFrom2 a (b-1) ++ [a^(b-1)]

--21)
isPrime :: Int -> Bool
isPrime n = isPrime2 n 2

isPrime2 :: Int -> Int -> Bool
isPrime2 n m | m^2 > n && m >= 2 = True
             | mod n m == 0 = False
             | otherwise = isPrime2 n (m+1)

--m ≤ √n
--m^2 <= √n^2
--m^2 <= n 
--Então seria verdade para m^2 > n 

--22)
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h:t) (h2:t2) | h == h2 = isPrefixOf t t2
                         | otherwise = False

--23)
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf (h:t) (h2:t2) | h == head t2 = isSuffixOf t t2
                         | otherwise = False
--25)
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices a l = elemConta a l 0

elemConta :: Eq a => a -> [a] -> Int -> [Int]
elemConta a [] n = []
elemConta a (h:t) n | a == h = n : elemConta a t (n+1)
                    | otherwise = elemConta a t (n+1)

--26)
nub :: Eq a => [a] -> [a]
nub [] = []
nub (h:t) | h `elem` t = nub t 
          | otherwise = h : nub t

--27)
delete :: Eq a => a -> [a] -> [a]
delete a [] = []
delete a (h:t) | a == h = t
               | otherwise = h : delete a t

--28)
remove :: Eq a => [a] -> [a] -> [a]
remove l [] = l
remove [] _ = []
remove l (h:t) = remove (delete h l) t

--delete, deleta a primeira ocurrencia de um numero expecifico

--29)
union :: Eq a => [a] -> [a] -> [a]
union l [] = l
union l (h:t)
    | h `elem` l = union l t
    | otherwise = union (l ++ [h]) t

--Importate
--É assim que se adiciona a ultima linha o elemento

--30)
intersect :: Eq a => [a] -> [a] -> [a]
intersect l [] = l
intersect [] l = []
intersect (h:t) l | h `elem` l = h : intersect t l
                  | otherwise = intersect t l 

--31)
inserte :: Ord a => a -> [a] -> [a]
inserte n [] = []
inserte n (h:t) | n > h && n < (head t) = h : n : inserte n t
                | otherwise = h : inserte n t

--32)
unwords1 :: [String] -> String
unwords1 [] = ""
unwords1 (h:t) = h ++ (if null t then "" else " ") ++ unwords1 t

--33)
unlines1 :: [String] -> String
unlines1 [] = ""
unlines1 (h:t) = h ++ "\n" ++ unlines1 t

--34)
pMaior :: Ord a => [a] -> Int
pMaior [_] = 0
pMaior (h:t)
    | h >= (t !! (pMaior t)) = 0
    | otherwise = 1 + pMaior t

--35)
lookup1 :: Eq a => a -> [(a,b)] -> Maybe b
lookup1 a [] = Nothing
lookup1 a ((x,y):t) | a == x = Just y
                    | otherwise = lookup1 a t

--36)
preCrescente1 :: Ord a => [a] -> [a]
preCrescente1 [] = []
preCrescente1 [x] = [x]
preCrescente1 (h:t) | h <= (head t) = h : preCrescente1 t
                    | otherwise = [h]

--37)
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = inserte h (iSort t)

--38)
menor :: String -> String -> Bool
menor _ "" = False
menor "" _ = True
menor a b | (length a) < (length b) = True
          | otherwise = False

--39)
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((x,y):t) | a == x = True
                     | otherwise = elemMSet a t 

--40)
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,1):t) = x : converteMSet t
converteMSet ((x,y):t) = x : converteMSet ((x,y-1) : t)

--41)
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((x,y):t) | a == x = (x,y+1) : t
                       | otherwise = (x,y) : insereMSet a t

--42)
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((x,y):t) | a == x && y > 1 = (x,y-1) : removeMSet a t
                       | a == x && y == 1 = removeMSet a t
                       | otherwise = (x,y) : removeMSet a t

--43)
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (l:ls) = insereMSet l (constroiMSet ls)

--44)
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left a):t) = (a : x, y)
    where (x,y) = partitionEithers t
partitionEithers ((Right a):t) = (x, a : y)
    where (x,y) = partitionEithers t

--45)
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Nothing):t) = catMaybes t
catMaybes ((Just a):t) = a : catMaybes t

data Movimento = Norte | Sul | Este | Oeste
    deriving Show

--46)
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (x1,y1) | x < x1 = Este : caminho (x+1,y) (x1,y1)     
                      | x > x1 = Oeste : caminho (x-1,y) (x1,y1)
                      | y < y1 = Norte : caminho (x,y+1) (x1,y1)
                      | y > y1 = Sul : caminho (x,y+1) (x1,y1)
                      | otherwise = []

--48)
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (h:t) | eQuadrado h = 1 + contaQuadrados t
                     | otherwise = contaQuadrados t

eQuadrado :: Rectangulo -> Bool
eQuadrado (Rect (x,y) (x1,y1)) = abs (y1 - y) == abs (x1 - x)

--49)
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2)):t) = abs (x2 - x1) * abs (y2 - y1) + areaTotal t

--50)
data Equipamento = Bom | Razoavel | Avariado
    deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (Avariado:t) = naoReparar t