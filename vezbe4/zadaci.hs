--1
rastaviString :: Char -> String -> [String]
rastaviString _ [] = []
rastaviString c s = rastaviStringPom s c ""
    where
        rastaviStringPom :: String -> Char -> String -> [String]
        rastaviStringPom [] _ akum = [akum]
        rastaviStringPom (h:t) delim akum
            | h == delim = akum : rastaviStringPom t delim ""
            | otherwise = rastaviStringPom t delim (akum ++ [h])

--2
spojiStringovi :: [String] -> String
spojiStringovi [] = ""
spojiStringovi l = foldl (\acc curr -> spoji acc curr) "" l
    where
        spoji :: String -> String -> String
        spoji "" b = b
        spoji a "" = a
        spoji a b = a ++ "," ++ b

--2a
-- spoji :: String -> String -> String
-- spoji [] = []
-- spoji [a] = a
-- spoji (x:xs) = x ++ ", " ++ spoji xs

rastaviPaSastavi :: [String] -> String
rastaviPaSastavi l = spojiStringovi(foldl (++) [] (map (rastaviString ' ') l))
-- rastaviPaSastavi l = agregiranaLista rastavljeni
--     where
--         rastavljeni = map (rastaviPaSastavi ' ') l
--         agregiranaLista [] = []
--         agregiranaLista (x:xs) = x ++ agregiranaLista xs

--4
svastaSaListom :: [[Int]] -> Int
svastaSaListom l = foldl (*) 1 sume
    where
        kvadriranaLista = map kvadriraj l
        sume = map sum kvadriranaLista

kvadriraj :: [Int] -> [Int]
kvadriraj [] = []
kvadriraj l = map (\x -> x * x) l

--5
data Naselje = 
    Selo {
        brojStanovnika :: Int,
        povrsina :: Double,
        tip :: String
    }
    | Varosica {
        brojStanovnika :: Int,
        povrsina :: Double
    }
    | Grad {
        brojStanovnika :: Int,
        povrsina :: Double,
        imaBazen :: Bool
    } deriving Show

--6
type Naselja = [Naselje]

izdvoji :: Naselja -> Naselja
izdvoji [] = []
izdvoji (h:t)
    | jeSelo h && (tip h) == "razbijeno" = h : izdvoji t --slucaj za sela
    | jeGrad h && (imaBazen h) && (brojStanovnika h) > 150000 = h : izdvoji t
    | otherwise = izdvoji t
    where
        jeSelo (Selo {}) = True 
        jeSelo _ = False
        jeGrad (Grad {}) = True
        jeGrad _ = False