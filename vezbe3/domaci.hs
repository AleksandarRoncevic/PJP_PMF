--1
data Stablo = Nista
    | Cvor {
        nosi :: Int,
        levo :: Stablo,
        desno :: Stablo
    } deriving Show

--2 
sadrzi :: Stablo -> Int -> Bool
sadrzi Nista _ = False
sadrzi s x
    | nosi s == x = True
    | otherwise = sadrzi (levo s) x || sadrzi (desno s) x

--3
uListu :: Stablo -> [Int]
uListu Nista = []
uListu s = uListu (levo s) ++ [nosi s] ++ uListu (desno s)

--4
deljiviSa3UListu :: Stablo -> [Int]
deljiviSa3UListu Nista = []
deljiviSa3UListu s 
    | mod info 3 == 0 || mod info 5 == 0 = nosi s : ostatak
    | otherwise = ostatak
    where
        ostatak = deljiviSa3UListu (levo s) ++ deljiviSa3UListu (desno s)
        info = nosi s

--5
uOgledalu :: Stablo -> Stablo
uOgledalu Nista = Nista
uOgledalu s = Cvor (nosi s) (uOgledalu (levo s)) (uOgledalu (desno s))

--6
filterStablo :: (Int -> Bool) -> Stablo -> [Int]
filterStablo _ Nista = []
filterStablo f s 
    | valid = ostatakL ++ [nosi s] ++ ostatakD
    | otherwise = ostatakL ++ ostatakD
    where
        valid = f (nosi s)
        ostatakL = filterStablo f (levo s)
        ostatakD = filterStablo f (desno s)