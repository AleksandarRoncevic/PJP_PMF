--1
ukloniPoslednjiElement :: [Int] -> [Int]
ukloniPoslednjiElement [] = []
ukloniPoslednjiElement [x] = []
ukloniPoslednjiElement (x:xs) = x : ukloniPoslednjiElement xs
-- ukloniPoslednjiElement (x:xs) = x : ukloniPoslednjiElement xs  alternativna verzija

--2
ukloniPretposlednjiElement :: [Int] -> [Int]
ukloniPretposlednjiElement [] = []
ukloniPretposlednjiElement [x] = [x]
ukloniPretposlednjiElement [x,y] = [y]
ukloniPretposlednjiElement (h:t) = h : ukloniPretposlednjiElement t

--3
--a)
faktorijel :: Int -> Int
faktorijel 0 = 1
faktorijel n = faktorijel (n - 1) * n

--b)
faktorijelRepno :: Int -> Int
faktorijelRepno 0 = 1
faktorijelRepno n = faktorijelPom n 1

faktorijelPom :: Int -> Int -> Int
faktorijelPom 0 akum = akum
faktorijelPom n akum = faktorijelPom (n - 1) ( akum * n )

--4
imaVelikaSlova :: [Char] -> Bool
imaVelikaSlova [] = False
imaVelikaSlova (x:xs)
    | isAsciiUpper x = True
    | otherwise            = imaVelikaSlova xs

isAsciiUpper :: Char -> Bool
isAsciiUpper c = c >= 'A' && c <= 'Z'
-- ovi guardovi su kao switch case ide redom kroz proizvoljno mnogo
-- slucajeva a poslednji 'otherwise' je default ako nista nije true 

--5 
spljosti :: [Int] -> [Int]
spljosti x = x


--6


--7
kvadriraj :: [Int] -> [Int]
kvadriraj l = [ x * x | x <- l ]


--10
daLiJeNeparan :: Int -> Bool
daLiJeNeparan n = mod n 2 == 1


filter' f [] = []
filter' f (x:xs)
    | f x       = filter' f xs
    | otherwise = x : filter' f xs

--primer koriscenja filter'
-- filter' daLiJeNeparan [1..15]