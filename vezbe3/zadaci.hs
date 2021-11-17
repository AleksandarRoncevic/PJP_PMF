--1

data Element a = Empty
    | Cvor a (Element a)
    deriving Show

data Stablo a = List
    | CvorStabla a (Stablo a) (Stablo a)
    deriving Show
--2

kreirajMojuListu :: [Int] -> Element Int
kreirajMojuListu [] = Empty
kreirajMojuListu (h:t) = Cvor h (kreirajMojuListu t)


--3
duzinaListe :: Element a -> Int
duzinaListe Empty = 0
duzinaListe (Cvor _ sledeci) = 1 + duzinaListe sledeci

--4 
--Eq znaci bilo koji tip/vrednost koji ima definisano poredjenje
uListi :: Eq a => a -> Element a -> Bool
uListi x Empty = False
uListi x (Cvor y sledeci) = x == y || uListi x sledeci

--5
data Planeta = Nista
    | Planeta {
        ime :: String,
        precnik :: Double,
        gasovita :: Bool
    } deriving Show

--6
type Planete = [Planeta]

--7
nadjiPoImenu :: String -> Planete -> Planeta
nadjiPoImenu _ [] = Nista
nadjiPoImenu imePlanete (h:t)
    | imePlanete == ime h = h
    | otherwise = nadjiPoImenu imePlanete t

nadjiPoImenu' :: String -> Planete -> Planeta
nadjiPoImenu' imePlanete l
    | null filtriranaLista = Nista
    | otherwise                   = head filtriranaLista
    where
        filtriranaLista = filter (\x -> ime x == imePlanete ) l

nadjiPoImenu'' :: String -> Planete -> Planeta
nadjiPoImenu'' imePlanete = foldr (\p accum -> if imePlanete == ime p then p else accum) Nista

--8

vratiGasovite :: Planete -> Planete
vratiGasovite = filter gasovita