import Data.Char  (toUpper, isLower)


--1
kvadIliPuta10 :: [Int] -> [Int]
kvadIliPuta10 [] = []
kvadIliPuta10 l
    | even (length l) = map (\x -> x * x) l
    | otherwise = map (* 10) l

--2
izbaciPaPovecaj :: [Char] -> [Char]
izbaciPaPovecaj [] = []
izbaciPaPovecaj s = map toUpper (filter isLower s)
-- izbaciPaPovecaj s = map toUpper (filter (\c -> c < 'z' && c >'a') s) identicna funckija

--3
f3 :: ([[Int]] -> [[Int]]) -> ([[Int]] -> [Int]) -> [[Int]] -> [Int]
f3 _ _ [] = []
f3 f1 f2 l = f2 $ f1 l

--4
izbaciParne' :: [[Int]] -> [[Int]]
izbaciParne' [] = []
izbaciParne' (h:t)
    | null neparni = izbaciParne' t
    | otherwise = neparni : izbaciParne' t
    where
        neparni = filter odd h

sumaPodniza' :: [[Int]] -> [Int]
sumaPodniza' = map sum

izbaciParnePaSumiraj = f3 izbaciParne' sumaPodniza'
--obrnuto su poredjane funkciju u odnosu na redosled izvrsavanja
--prvo ide izbaciParne pa onda sumaPodniza

--5

prosecnaDuzina :: [[Char]] -> Int
prosecnaDuzina [] = 0
prosecnaDuzina l = div sumaDuzina (length l)
    where
        duzine = map length l
        sumaDuzina = foldl (+) 0 duzine