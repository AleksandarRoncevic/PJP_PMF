--1 concat vis
ispeglaj :: [[Int]] -> [Int]
ispeglaj [] = []
ispeglaj (h:t) = h ++ ispeglaj t
---
ispeglaj' :: [[Int]] -> [Int]
ispeglaj' [] = []
ispeglaj' l = ispeglajPom l []

ispeglajPom :: [[Int]] -> [Int] -> [Int]
ispeglajPom [] akum = akum
ispeglajPom (h:t) akum = ispeglajPom t (akum ++ h)
---
ispeglaj'' :: Foldable t => t [a] -> [a]
ispeglaj'' l = foldl (++) [] l

--2
sumaPodniza :: [[Int]] -> [Int]
sumaPodniza [] = []
sumaPodniza (h:t) = suma : sumaPodniza t
    where
        suma = foldl (+) 0 h

sumaPodniza' :: [[Int]] -> [Int]
sumaPodniza' = map sum
--za svaki element/podniz odraditi funkciju sum

--3
izbaciParne :: [[Int]] -> [[Int]]
izbaciParne [] = []
izbaciParne l = map (filter odd) l

izbaciParne' :: [[Int]] -> [[Int]]
izbaciParne' [] = []
izbaciParne' (h:t)
    | null neparni = izbaciParne' t
    | otherwise = neparni : izbaciParne' t
    where
        neparni = filter odd h

izbaciParne'' l = map (filter odd) (filter (any odd) l)

--filter (any odd) proverava da li podlista ima barem jedan
-- neparni element posto ako nema ostace nam prazna lista te tako 
--izbegava prazne liste nakon drugog filtriranja koje je samo (filter odd)

--4
okreni :: [[Char]] -> [[Char]]
okreni [] = []
okreni l = map reverse l

--5
izbaciDeljiveSa3 :: [[Int]] -> [[Int]]
izbaciDeljiveSa3 [] = []
izbaciDeljiveSa3 l = filter (/= [])(map (filter (\x -> mod x 3 /= 0)) l)

--6