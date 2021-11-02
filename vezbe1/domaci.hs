--1
ukloniSvakiNti :: [Int] -> Int -> [Int]
ukloniSvakiNti [] n = []
ukloniSvakiNti x n = ukloni x n 1

ukloni :: [Int] -> Int -> Int -> [Int]
ukloni [] n _ = []
ukloni (x:xs) n k
    | n == k = ukloni xs n 1
    | otherwise = x : ukloni xs n (k+1)

--2
sviDeliociBroja :: Int -> [Int]
sviDeliociBroja 0 = []
sviDeliociBroja n = nadjiDelioce n 1

nadjiDelioce :: Int -> Int -> [Int]
nadjiDelioce n k
    | k < div n 2 = if mod n k == 0 then k : nadjiDelioce n (k+1) else nadjiDelioce n (k+1)
    | otherwise = [k | mod n k == 0] ++ [n]

--3 quicksort
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerSorted = quickSort [a | a <- xs, a <= x]
        biggerSorted = quickSort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

--4 filter''
-- filter'' :: (Ord a) => [a] -> [a]
filter'' f [] = []
filter'' f x = [a | a <- x, f a]

--5
sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista x = sumiraj x 0

sumiraj :: [Int] -> Int -> Int
sumiraj [] sum = sum
sumiraj (x:xs) sum = sumiraj xs (sum+x)

--6
zip' :: [Int] -> [Int] -> [Int]
zip' [] [] = []
zip' [] x = x
zip' x [] = x
zip' (x:xs) (y:ys) = (x+y) : zip' xs ys

--7 a)

sumaCifara :: Int -> Int
sumaCifara x
    | x >= 10 = mod x 10 + sumaCifara (div x 10)
    | otherwise = x

-- b)
sumaCifaraRepno :: Int -> Int
sumaCifaraRepno x = sumaCifaraRepnoPom x 0

sumaCifaraRepnoPom :: Int -> Int -> Int
sumaCifaraRepnoPom x akum
    | x >= 10 =
        let ost = mod x 10
            del = div x 10
        in sumaCifaraRepnoPom del (akum+ost)
    | otherwise = x + akum

--8
sumiraParneCifre :: Int -> Int
sumiraParneCifre x
    | x >= 10 = sumiraParne x 0
    | otherwise = x

sumiraParne :: Int -> Int -> Int
sumiraParne 0 akum = akum
sumiraParne x akum
    | even x = sumiraParne (div x 10) (mod x 10 + akum)
    | otherwise = sumiraParne (div x 10) akum

