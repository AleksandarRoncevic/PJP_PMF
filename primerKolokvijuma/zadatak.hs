{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (elemIndex, sortBy)
--1
svakiDrugiZnak :: [[Int]] -> [[Int]]
svakiDrugiZnak [] = []
svakiDrugiZnak l = map menjajZnak l
    where
        menjajZnak [] = []
        menjajZnak [x] = [-x]
        menjajZnak (x:y:xs)= [-x,y] ++ menjajZnak xs

sumaIfilter :: [[Int]] -> [Int]
sumaIfilter [] = []
sumaIfilter l =  filter (> 99) (map sumirajParne novaLista)
    where
        sumirajParne = foldl (\sum curr -> if even curr then sum + curr else sum) 0
        novaLista = svakiDrugiZnak l
--2
data StambeniObjekat =
    GradskaKuca {
        kvadratura :: Double,
        kapacitet :: Int
    }
    | SeoskaKuca {
        kvadratura :: Double,
        kapacitet :: Int,
        dvoriste :: Double
    }
    | Zgrada {
        kvadratura :: Double,
        kapacitet :: Int,
        spratovnost :: Int
    }

type StamObjekti = [StambeniObjekat]

izdvojObjekte :: StamObjekti -> StamObjekti
izdvojObjekte [] = []
izdvojObjekte (x:xs)
    |  isValid x = x : izdvojObjekte xs
    |  isValid x && dv x > 5 = x : izdvojObjekte xs
    |  isValid x && sprat x > 6 = x : izdvojObjekte xs
    | otherwise = izdvojObjekte xs
    where
        ka x = kapacitet x
        dv x = dvoriste x
        sprat x = spratovnost x
        isValid GradskaKuca {} = True
        isValid SeoskaKuca {} = True
        isValid Zgrada {} = True

saberiKapacitete :: StamObjekti -> Int
saberiKapacitete [] = 0
saberiKapacitete l = foldl (\sum curr -> sum + kapacitet curr) 0 l

--3

glavna :: [(Double,Double)] -> Int -> [((Double,Double),Int)]
glavna [] _ = []
glavna listaTacaka k
    | length listaTacaka < k = clusters
    | otherwise              = prvaIteracija listaTacaka k centralneTacke clusters
    where
        centralneTacke = take k listaTacaka
        clusters = zip centralneTacke [1..k]


prvaIteracija :: [(Double, Double)]-> Int -> [(Double, Double)] -> [((Double, Double), Int)]  -> [((Double, Double), Int)]
prvaIteracija (x:xs) k centralneTacke clusters = prvaIteracija xs k centralneTacke (clusters ++ novaTacka)
    where
        closestClusterIndex = minDist x centralneTacke
        novaTacka = zip [x] [closestClusterIndex]

prvaIteracija [] k centralneTacke clusters =  sortBy (\ (_,a) (_,b) -> compare a b) clusters


minDist :: (Double,Double) -> [(Double,Double)] -> Int
minDist p l = vratiIndex najmanja listaDistanci 0
    where
        listaDistanci = map (`distance` p) l 
        najmanja = minimum listaDistanci

vratiIndex min (x:xs) k
    | min == x = k
    | otherwise = vratiIndex min xs k+1


distance :: (Double,Double) -> (Double,Double) -> Double
distance (x1,y1) (x2,y2) = sqrt(dx + dy)
    where
        dx = (x1 - x2)^2
        dy = (y1 - y2)^2