{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

izbaciProste :: [Int] -> [Int]
izbaciProste [] = []
izbaciProste l = filter nijeProst l

nijeProst :: Int -> Bool
nijeProst 1 = True
nijeProst x = [1,x] == [n | n <- [1..x], x `mod` n == 0]

glavna :: [Int] -> [Int]
glavna [] = []
glavna l = map (\x -> x `mod` length l)(izbaciProste l)

data Stednja =
    DinarskaStednja {
        kamata :: Double,
        orocena :: Bool
    }
    |
    DeviznaStednja {
        kamata :: Double,
        orocena :: Bool,
        uEvrima :: Bool
    } deriving Show

type Stednje = [Stednja]

glavna2 :: Stednje -> Stednje
glavna2 [] = []
glavna2 (x:xs)
    | jeDinarska x && kamata x > 2.9 = ubaci
    | jeDevizna x && orocena x && kamata x > 1 = ubaci
    | jeDevizna x && orocena x && not (uEvrima x) = ubaci
    | otherwise = glavna2 xs
    where
        jeDinarska DinarskaStednja {} = True
        jeDinarska _ = False
        jeDevizna DeviznaStednja {} = True
        jeDevizna _ = False
        ubaci = x : glavna2 xs
--Test slucaj
--a = DinarskaStednja { kamata = 3, orocena = False } 
--b = DinarskaStednja { kamata = 2, orocena = False } 
--c = DeviznaStednja {kamata = 1.5, orocena = True, uEvrima = True }
--d = DeviznaStednja {kamata = 0.9, orocena = True, uEvrima = False }
--e = DeviznaStednja {kamata = 0.8, orocena = True, uEvrima = True}

--3


nadjiDet :: [[Int]] -> Int
nadjiDet a
    | not (kvadratna a) = -1
    | otherwise = izracunajDeterminantu a


izracunajDeterminantu :: [[Int]] -> Int
izracunajDeterminantu [x,y] = head x * y !! 1 - head y * x !! 1
izracunajDeterminantu a = sum $ map (\kol -> ((a !! 0)!!kol)*(sgn kol)*(minorAij a kol)) [0..n]
    where
        n = length a - 1
        sgn kol = if even kol then 1 else -1

izbaci :: [[Int]] -> Int -> [[Int]]
izbaci a k = map (\x -> take k x++drop (k+1) x) a

minorAij :: [[Int]] -> Int -> Int
minorAij a k = izracunajDeterminantu (izbaci ( drop 1 a) k)

kvadratna :: [[Int]] -> Bool
kvadratna [] = True
kvadratna l =  length l == prvi && jednakeKolone
    where
        prvi = length (head l)
        jednakeKolone = all (== head duzineKolona) duzineKolona --proveravamo da li su medjusobno jednake
        duzineKolona = map length l --dobijamo duzine svih kolona