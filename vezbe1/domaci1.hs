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