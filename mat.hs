import Data.List
import Data.Maybe
import Data.Char(digitToInt)


-- "d7" = Policko 4 7, "a2" = Policko 1 2 atd.
data Policko = Policko Int Int
  deriving (Show, Eq)

-- druh figurky a jej policko
data Pozicia = K Policko | D Policko | V Policko
  deriving (Show, Eq)

-- Tah Kd6 Ve2, TahZaver De5# -
data Tah = Tah Pozicia Bool Pozicia | TahZaver Pozicia

data Smer = Riadok | Stlpec

-- >>> Tah (K (Policko 3 6)) (V (Policko 5 2))
-- Kc6     Ve2
-- >>> TahZaver (D (Policko 2 7))
-- Db7#
--
instance Show Tah where
  show (Tah biely sach cierny) = 
    if sach == True 
    then (poziciaNaString biely) ++ "+    "++(poziciaNaString cierny)
    else (poziciaNaString biely) ++ "     "++(poziciaNaString cierny)
  show (TahZaver biely) = (poziciaNaString biely) ++ "#"

-- hlavna funkcia, testy na konci
dajMat :: [[Char]] -> [Char] -> IO()
dajMat biely cierny = vypisVysledok (zbierajTahy ([polickaPreFigurku (prelozStringNaPoziciu x) | x <- biely],polickaPreFigurku (prelozStringNaPoziciu cierny)))

-- >>> vypisVysledok [Tah (D (Policko 4 4)) True (K (Policko 1 3)), TahZaver (D (Policko 1 4))]
-- Dd4+    Ka3
-- Da4#
vypisVysledok :: [Tah] -> IO()
vypisVysledok [] = return ()
vypisVysledok (x:xs) = do putStrLn (show x)
                          vypisVysledok xs


-- >>> prelozStringNaPoziciu "De2"
-- D (Policko 5 2)
prelozStringNaPoziciu :: [Char] -> Pozicia
prelozStringNaPoziciu ('K': pismeno: cislo: []) = K (Policko (pismenkoNaCislo pismeno) (digitToInt cislo))
prelozStringNaPoziciu ('D': pismeno: cislo: []) = D (Policko (pismenkoNaCislo pismeno) (digitToInt cislo))
prelozStringNaPoziciu ('V': pismeno: cislo: []) = V (Policko (pismenkoNaCislo pismeno) (digitToInt cislo))
prelozStringNaPoziciu _ = undefined


-- usporiada figurky podla 1) "pohyblivosti" 2) pozicie od policka a1
instance Ord Pozicia where
  compare (K (Policko a b)) (K (Policko c d)) = compare (a,b) (c,d)
  compare (V (Policko a b)) (V (Policko c d)) = compare (a,b) (c,d)
  compare (D (Policko a b)) (D (Policko c d)) = compare (a,b) (c,d)
  compare (K _) (V _) = LT
  compare (V _) (K _) = GT
  compare (V _) (D _) = LT
  compare (D _) (V _) = GT
  compare (K _) (D _) = LT
  compare (D _) (K _) = GT

listNaTuple :: [a] -> (a,a)
listNaTuple [x,y] = (x,y)
listNaTuple _ = undefined

pismenaPolicok :: [Char]
pismenaPolicok = "abcdefgh"

-- >>> poziciaNaString (D (Policko 1 1))
-- "Da1"
--
poziciaNaString :: Pozicia -> String
poziciaNaString (K (Policko a b)) = "K"++[cisloNaPismenko a]++(show b)
poziciaNaString (V (Policko a b)) = "V"++[cisloNaPismenko a]++(show b)
poziciaNaString (D (Policko a b)) = "D"++[cisloNaPismenko a]++(show b)


-- skonvertuje pismeno na prisluchajuce cislo 
-- >>> pismenkoNaCislo 'b'
-- 2
--
pismenkoNaCislo :: Char -> Int
pismenkoNaCislo x = (fromJust $ (elemIndex x pismenaPolicok)) + 1

-- skonvertuje cislo na prisluchajuce pismeno
-- >>> cisloNaPismenko 7
-- 'g'
--
cisloNaPismenko :: Int -> Char
cisloNaPismenko x = pismenaPolicok !! (x - 1)

zoznamTupleNaPolicka  :: [(Int, Int)] -> [Policko]
zoznamTupleNaPolicka zoz = [Policko (fst z) (snd z) | z <- zoz]

zoznamPolicokNaTuple :: [Policko] -> [(Int, Int)]
zoznamPolicokNaTuple pole = [(a,b)|(Policko a b) <- pole]

-- podla pozicie krala/veze/damy urci vsetky policka, na ktore sa mozu potencialne pohnut, neriesime pozicie ostatnych figuriek na sachovnici
-- >>> polickaPreKrala (Policko 8 2)
-- [Policko 7 1,Policko 7 2,Policko 7 3,Policko 8 3,Policko 8 1]
--
polickaPreKrala :: Policko -> [Policko]
polickaPreKrala (Policko x y) =
  case (x,y) of (1,1) -> zoznamTupleNaPolicka  [(1,2),(2,1),(2,2)]
                (8,1) -> zoznamTupleNaPolicka  [(7,1),(7,2),(8,2)]
                (1,8) -> zoznamTupleNaPolicka  [(1,7),(2,7),(2,8)]
                (8,8) -> zoznamTupleNaPolicka  [(7,8),(7,7),(8,7)]
                (8,a) -> zoznamTupleNaPolicka  ((zip [7,7,7] [(a-1)..(a+1)]) ++ [(8,a+1),(8,a-1)])
                (a,1) -> zoznamTupleNaPolicka  ((zip [(a-1)..(a+1)] [2,2,2]) ++ [(a+1,1),(a-1,1)])
                (1,a) -> zoznamTupleNaPolicka  ((zip [2,2,2] [(a-1)..(a+1)]) ++ [(1,a+1),(1,a-1)])
                (a,8) -> zoznamTupleNaPolicka  ((zip [(a-1)..(a+1)] [7,7,7] ) ++ [(a+1,8),(a-1,8)])
                (a,b) -> zoznamTupleNaPolicka  ((zip [a-1,a-1,a-1] [(b-1)..(b+1)]) ++ (zip [a+1,a+1,a+1] [(b-1)..(b+1)]) ++ [(a,b-1),(a,b+1)])

-- >>> polickaPreVezu (Policko 2 7)
-- [Policko 1 7,Policko 3 7,Policko 4 7,Policko 5 7,Policko 6 7,Policko 7 7,Policko 8 7,
-- Policko 2 1,Policko 2 2,Policko 2 3,Policko 2 4,Policko 2 5,Policko 2 6,Policko 2 8]
--
polickaPreVezu :: Policko -> [Policko]
polickaPreVezu (Policko x y) = let zoz = (zoznamTupleNaPolicka ((zip [1..8] (take 8 (cycle [y]))) ++ (zip (take 8 (cycle [x])) [1..8])))
                       in filter (/= (Policko x y)) zoz

-- >>> polickaPreDamu (Policko 1 1)
-- [Policko 2 2,Policko 3 3,Policko 4 4,Policko 5 5,Policko 6 6,Policko 7 7,Policko 8 8,
-- Policko 2 1,Policko 3 1,Policko 4 1,Policko 5 1,Policko 6 1,Policko 7 1,Policko 8 1,
-- Policko 1 2,Policko 1 3,Policko 1 4,Policko 1 5,Policko 1 6,Policko 1 7,Policko 1 8]
--
polickaPreDamu :: Policko -> [Policko]
polickaPreDamu (Policko x y) = 
    if x>y
    then if x+y <= 8
        then let zoz = zoznamTupleNaPolicka ((zip [(x-y+1)..8] [1..8]) ++ (zip [1..8] [(x+y-1),(x+y-2)..1])) ++ (polickaPreVezu (Policko x y))
              in filter (/= (Policko x y)) zoz
        else let zoz = zoznamTupleNaPolicka ((zip [(x-y+1)..8] [1..8]) ++ (zip [(x+y-8),(x+y-7)..8] [8,7..1])) ++ (polickaPreVezu (Policko x y))
             in filter (/= (Policko x y)) zoz
    else if x+y <= 8
        then let zoz = zoznamTupleNaPolicka ((zip [1..8] [(y-x+1)..8]) ++ (zip [1..8] [(x+y-1),(x+y-2)..1])) ++ (polickaPreVezu (Policko x y))
              in filter (/= (Policko x y)) zoz
        else let zoz = zoznamTupleNaPolicka ((zip [1..8] [(y-x+1)..8]) ++ (zip [(y+x-8)..8] [8,7..1])) ++ (polickaPreVezu (Policko x y))
              in filter (/= Policko x y) zoz

-- z pozicie figurky vytvori tuple (Pozicia, [policka, na ktore sa moze pohnut])
-- >>> polickaPreFigurku (K (Policko 1 2))
-- (K (Policko 1 2),[Policko 2 1,Policko 2 2,Policko 2 3,Policko 1 3,Policko 1 1])
--
polickaPreFigurku :: Pozicia -> (Pozicia, [Policko])
polickaPreFigurku poz@(K policko) = (poz, polickaPreKrala policko)
polickaPreFigurku poz@(V policko) = (poz, polickaPreVezu policko)
polickaPreFigurku poz@(D policko) = (poz, polickaPreDamu policko)

-- odfiltruje zadane policka zo zoznamu moznych poli
odfiltrujBlokPolia :: (Pozicia, [Policko]) -> [Policko] -> (Pozicia, [Policko])
odfiltrujBlokPolia x [] = x
odfiltrujBlokPolia my (s:super) = odfiltrujBlokPolia (filtruj my s) super

-- odstrani policka, ktore su v zakryte inej figury
-- napr. pre Ve2, De4 pre vezu odstrani policka e4-e8 
-- >>>filtruj (V (Policko 1 2), [(Policko x y) | x <- [1], y <- [3..8]]) (Policko 1 6)
-- (V (Policko 1 2),[Policko 1 3,Policko 1 4,Policko 1 5,Policko 1 6])
--
filtruj :: (Pozicia, [Policko]) -> Policko -> (Pozicia, [Policko])
filtruj figurka@(D (Policko x y), policka) (Policko xSuper ySuper)
-- pre damu
  | x > xSuper && y < ySuper = (D (Policko x y), [(Policko a b)| (Policko a b) <- policka, a >= xSuper || b <= ySuper])
  | x < xSuper && y < ySuper = (D (Policko x y), [(Policko a b)| (Policko a b) <- policka, a <= xSuper || b <= ySuper])
  | x > xSuper && y > ySuper = (D (Policko x y), [(Policko a b)| (Policko a b) <- policka, a >= xSuper || b >= ySuper])
  | x < xSuper && y > ySuper = (D (Policko x y), [(Policko a b)| (Policko a b) <- policka, a <= xSuper || b >= ySuper])
  | x == xSuper && y > ySuper = (D (Policko x y), [(Policko a b)| (Policko a b) <- policka, a /= xSuper || b >= ySuper])
  | x == xSuper && y < ySuper = (D (Policko x y), [(Policko a b)| (Policko a b) <- policka, a /= xSuper || b <= ySuper])
  | x < xSuper && y == ySuper = (D (Policko x y), [(Policko a b)| (Policko a b) <- policka, a <= xSuper || b /= ySuper])
  | x > xSuper && y == ySuper = (D (Policko x y), [(Policko a b)| (Policko a b) <- policka, a >= xSuper || b /= ySuper])
  | otherwise = figurka

filtruj figurka@(V (Policko x y), policka) (Policko xSuper ySuper)
-- pre vezu
  | x == xSuper && y > ySuper = (V (Policko x y), [(Policko a b)| (Policko a b) <- policka, a /= xSuper || b >= ySuper])
  | x == xSuper && y < ySuper = (V (Policko x y), [(Policko a b)| (Policko a b) <- policka, a /= xSuper || b <= ySuper])
  | x < xSuper && y == ySuper = (V (Policko x y), [(Policko a b)| (Policko a b) <- policka, a <= xSuper || b /= ySuper])
  | x > xSuper && y == ySuper = (V (Policko x y), [(Policko a b)| (Policko a b) <- policka, a >= xSuper || b /= ySuper])
  | otherwise = figurka

filtruj figurka@(K _, _) _ = figurka 

-- >>> polePozicie (D (Policko 7 4), [])
-- Policko 7 4
--
polePozicie :: (Pozicia, [Policko]) -> Policko
polePozicie ((K (Policko x y), _)) = Policko x y
polePozicie ((D (Policko x y)),_) = Policko x y
polePozicie ((V (Policko x y)),_) = Policko x y

-- zoznam vsetkych policok, ktore ohrozuje nejaka figurka
-- >>> vsetkyOhrozenePolia [polickaPreFigurku x | x <- [K (Policko 2 7), V (Policko 2 4), D (Policko 2 3)]]
-- [Policko 1 6,Policko 1 7,Policko 1 8,Policko 3 6,Policko 3 7,Policko 3 8,Policko 2 6,Policko 2 8,Policko 1 4,
-- Policko 3 4,Policko 4 4,Policko 5 4,Policko 6 4,Policko 7 4,Policko 8 4,Policko 2 1,Policko 2 2,Policko 2 3,
-- Policko 2 5,Policko 2 6,Policko 2 7,Policko 2 8,Policko 1 2,Policko 3 4,Policko 4 5,Policko 5 6,Policko 6 7,
-- Policko 7 8,Policko 1 4,Policko 3 2,Policko 4 1,Policko 1 3,Policko 3 3,Policko 4 3,Policko 5 3,Policko 6 3,
-- Policko 7 3,Policko 8 3,Policko 2 1,Policko 2 2,Policko 2 4,Policko 2 5,Policko 2 6,Policko 2 7,Policko 2 8]
--
vsetkyOhrozenePolia :: [(Pozicia, [Policko])] -> [Policko]
vsetkyOhrozenePolia [] = []
vsetkyOhrozenePolia ((_,policka):xs) = policka ++ (vsetkyOhrozenePolia xs)

-- >>> jeSach ([polickaPreFigurku (D (Policko 2 1))], polickaPreFigurku (K (Policko 4 1)))
-- True
--
jeSach :: ([(Pozicia, [Policko])],(Pozicia, [Policko])) -> Bool
jeSach (polia, (K policko, _))
 | elem policko zoznam = True
 | otherwise = False
 where zoznam = vsetkyOhrozenePolia polia
jeSach _ = undefined

-- >>> jeMat (napadnutePoliaFigur ([polickaPreFigurku (K (Policko 2 2)), polickaPreFigurku (D (Policko 4 2))], (polickaPreFigurku (K (Policko 4 1)))))
-- False
--
jeMat :: ([(Pozicia, [Policko])],(Pozicia, [Policko])) -> Bool
jeMat pozicia
 | (jeSach pozicia == True) && (snd (snd situacia) == []) = True
 | otherwise = False
 where situacia = napadnutePoliaFigur pozicia
 
-- >>> zoradPolicka ([polickaPreFigurku x | x <- [D (Policko 2 2), K (Policko 3 2)]]) (polickaPreFigurku (K (Policko 5 3)))
-- (D (Policko 2 2),[Policko 5 2,Policko 4 2,Policko 3 3,Policko 2 3,Policko 3 2,Policko 1 3,Policko 3 1,Policko 1 2,Policko 2 1,Policko 1 1])
--
zoradPolicka :: [(Pozicia, [Policko])] -> (Pozicia, [Policko]) -> (Pozicia, [Policko])
zoradPolicka [dama@(D (poziciaD), polickaD), bkral] ckral@(kpoz,_) = (D (poziciaD), [p | p <- utriedenePole, (stvorec p kraj) /= 6])
 where zoznam = [policko | policko <- polickaD, damaZaKralom dama policko kpoz]
       kraj = zatlacovaciKraj (polePozicie ckral) dama
       utriedenePole = (sortBy (\a b -> if (stvorec a kraj) == (stvorec b kraj)
                                        then compare (vzdialenost (polePozicie bkral) b) (vzdialenost (polePozicie bkral) a)
                                        else compare (stvorec a kraj) (stvorec b kraj)) zoznam)

zoradPolicka [(K (poziciaK), polickaK),(D (damapoz), _)] ckral@(K (_), cpolicka) = (K (poziciaK), (sortBy (\a b -> compare (stvorec a kraj) (stvorec b kraj)) bezppolicka))
 where kraj = zatlacovaciKraj (polePozicie ckral) (polickaPreFigurku (D (Policko 4 4)))
       bezppolicka = [p | p <- polickaK, 
                          not (p `elem` cpolicka), 
                          ((vzdialenostHoriz p kraj) > (vzdialenostHoriz damapoz kraj)) ||
                          ((vzdialenostVert p kraj) > (vzdialenostVert damapoz kraj))]
zoradPolicka _ _ = undefined

-- >>> poziciaNaPoziciu (polickaPreFigurku (D (Policko 2 3))) (Policko 2 7)
-- (D (Policko 2 7),[Policko 1 6,Policko 3 8,Policko 1 8,Policko 3 6,Policko 4 5,Policko 5 4,Policko 6 3,
-- Policko 7 2,Policko 8 1,Policko 1 7,Policko 3 7,Policko 4 7,Policko 5 7,Policko 6 7,Policko 7 7,Policko 8 7,
-- Policko 2 1,Policko 2 2,Policko 2 3,Policko 2 4,Policko 2 5,Policko 2 6,Policko 2 8])
--
poziciaNaPoziciu :: (Pozicia, [Policko]) -> Policko -> (Pozicia, [Policko])
poziciaNaPoziciu (K (_), _) policko2 = polickaPreFigurku (K policko2)
poziciaNaPoziciu (V (_),_) policko2 = polickaPreFigurku (V policko2)
poziciaNaPoziciu (D (_),_) policko2 = polickaPreFigurku (D policko2)

-- >>> vzdialenost (Policko 2 7) (Policko 6 3)
-- 8
--
vzdialenost :: Policko -> Policko -> Int
vzdialenost a b = (vzdialenostHoriz a b) + (vzdialenostVert a b)   

-- >>> vzdialenostHoriz (Policko 2 7) (Policko 6 3)
-- 4
--
vzdialenostHoriz :: Policko -> Policko -> Int
vzdialenostHoriz (Policko a _) (Policko b _) = abs (a - b)

-- >>> vzdialenostVert (Policko 2 7) (Policko 6 3)
-- 4
--
vzdialenostVert :: Policko -> Policko -> Int
vzdialenostVert (Policko _ a) (Policko _ b) = abs (a - b)

-- >>> stvorec (Policko 2 7) (Policko 6 3)
-- 25
--
stvorec :: Policko -> Policko -> Int
stvorec a b = ((vzdialenostHoriz a b) + 1) * ((vzdialenostVert a b) + 1)

-- >>> zatlacovaciKraj (Policko 3 2) (polickaPreFigurku (D (Policko 2 4)))
-- Policko 8 1
--
zatlacovaciKraj :: Policko -> (Pozicia, [Policko]) -> Policko
zatlacovaciKraj (Policko cx cy) pozicia
 | (cx >= dx) && (cy >= dy) = (Policko 8 8)
 | (cx >= dx) && (cy <= dy) = (Policko 8 1)
 | (cx <= dx) && (cy >= dy) = (Policko 1 8)
 | (cx <= dx) && (cy <= dy) = (Policko 1 1)
 where (Policko dx dy) = polePozicie pozicia
zatlacovaciKraj _ _ = undefined

-- >>> damaZaKralom (polickaPreFigurku (D (Policko 1 3))) (Policko 6 3) (K (Policko 7 5))
-- True
--

damaZaKralom :: (Pozicia, [Policko]) -> Policko -> Pozicia -> Bool
damaZaKralom dama policko (K (polickoK)) = 
  ((vzdialenostHoriz policko kraj) >= (vzdialenostHoriz polickoK kraj)) && ((vzdialenostVert policko kraj) >= (vzdialenostVert polickoK kraj))
  where kraj = zatlacovaciKraj polickoK dama
damaZaKralom _ _ _ = undefined

-- >>> bezpecnePolicko (polickaPreFigurku (V (Policko 6 4))) [polickaPreFigurku x | x <- [K (Policko 3 2), V (Policko 5 1)]] (polickaPreFigurku (K (Policko 6 3)))
-- False
--
bezpecnePolicko :: (Pozicia, [Policko]) -> [(Pozicia, [Policko])] -> (Pozicia, [Policko]) -> Bool
bezpecnePolicko  dama figurky ckral
 | not (policko `elem` (vsetkyOhrozenePolia [ckral])) = True
 | (policko `elem`  (vsetkyOhrozenePolia [ckral])) && (policko `elem` (vsetkyOhrozenePolia ostatneFigurky)) = True
 | otherwise = False
 where ostatneFigurky = [pozicia | pozicia <- zvysne, pozicia /= dama] ++ [polickaPreFigurku (fst kral)]
       policko = polePozicie dama
       kral = head [(K (a), b)| (K (a), b) <- figurky]
       zvysne = [x | x <- figurky, x /= kral]


-- >>> polickaMedziKralmi (polickaPreFigurku (K (Policko 8 4))) (polickaPreFigurku (K (Policko 8 8)))
-- [Policko 1 5,Policko 2 5,Policko 3 5,Policko 4 5,Policko 5 5,Policko 6 5,Policko 7 5,Policko 8 5,
-- Policko 1 6,Policko 2 6,Policko 3 6,Policko 4 6,Policko 5 6,Policko 6 6,Policko 7 6,Policko 8 6,Policko 1 7,
-- Policko 2 7,Policko 3 7,Policko 4 7,Policko 5 7,Policko 6 7,Policko 7 7,Policko 8 7]

polickaMedziKralmi :: (Pozicia, [Policko]) -> (Pozicia, [Policko]) -> [Policko]
polickaMedziKralmi biely cierny
 | by == cy = polickavert
 | bx == cx = polickahoriz
 | otherwise = polickahoriz ++ polickavert
 where (Policko bx by) = polePozicie biely
       (Policko cx cy) = polePozicie cierny
       polickavert = [(Policko x y) | x <- [((min bx cx) + 1)..((max bx cx) - 1)], y <- [1..8]]
       polickahoriz = [(Policko x y) | y <- [((min by cy) + 1)..((max by cy) - 1)], x <- [1..8]]

-- ako polickaMedziKralmi, ale vratane stlpcov a riadkov kralov
polickaMedziKralmi' :: (Pozicia, [Policko]) -> (Pozicia, [Policko]) -> [Policko]
polickaMedziKralmi' biely cierny
 | by == cy = polickavert
 | bx == cx = polickahoriz
 | otherwise = polickahoriz ++ polickavert
 where (Policko bx by) = polePozicie biely
       (Policko cx cy) = polePozicie cierny
       polickavert = [(Policko x y) | x <- [((min bx cx))..((max bx cx))], y <- [1..8]]
       polickahoriz = [(Policko x y) | y <- [((min bx cy))..((max by cy))], x <- [1..8]]


-- >>> matDaKZac (napadnutePoliaFigur ([polickaPreFigurku (D (Policko 3 2)), polickaPreFigurku (K (Policko 2 2))], (polickaPreFigurku (K (Policko 5 3)))))
-- (D (Policko 4 1),([(D (Policko 4 1),[Policko 5 2,Policko 6 3,Policko 7 4,Policko 8 5,Policko 2 3,Policko 3 2,Policko 1 1,Policko 2 1,Policko 3 1,
-- Policko 5 1,Policko 6 1,Policko 7 1,Policko 8 1,Policko 4 2,Policko 4 3,Policko 4 4,Policko 4 5,Policko 4 6,Policko 4 7,Policko 4 8]),
-- (K (Policko 2 2),[Policko 1 1,Policko 1 2,Policko 1 3,Policko 3 1,Policko 3 2,Policko 3 3,Policko 2 1,Policko 2 3])],
-- (K (Policko 5 3),[Policko 6 2,Policko 6 4,Policko 5 4])))
--
matDaKZac :: ([(Pozicia, [Policko])],(Pozicia, [Policko])) -> (Pozicia,([(Pozicia, [Policko])],(Pozicia, [Policko])))
matDaKZac ([dama, kral], ckral) = 
  if ((polePozicie dama) `elem` (polickaMedziKralmi kral ckral) || zoz == [])
  then matDaK ([dama, kral], ckral)
  else if (jeSach novakonstelacia) == True
       then if (jeMat novakonstelacia) == True
            then (fst (novapoz),novakonstelacia)
            else matDaKZac ([(fst dama, tail zoz), kral], ckral)
       else (fst (novapoz),novakonstelacia)
 where zoz = [x | x <- (polickaMedziKralmi kral ckral), x `elem` (snd dama)]
       novapoz = poziciaNaPoziciu dama (head zoz)
       novakonstelacia = napadnutePoliaFigur ([novapoz, kral],ckral)
matDaKZac _ = undefined

-- >>> matDaK (napadnutePoliaFigur ([polickaPreFigurku x | x <- [D (Policko 4 4),K (Policko 3 2)]],polickaPreFigurku (K (Policko 6 3))))
-- (K (Policko 3 1),([(D (Policko 4 4),[Policko 1 1,Policko 2 2,Policko 3 3,Policko 5 5,Policko 6 6,Policko 7 7,Policko 8 8,Policko 1 7,Policko 2 6,
-- Policko 3 5,Policko 5 3,Policko 6 2,Policko 7 1,Policko 1 4,Policko 2 4,Policko 3 4,Policko 5 4,Policko 6 4,Policko 7 4,Policko 8 4,Policko 4 1,
-- Policko 4 2,Policko 4 3,Policko 4 5,Policko 4 6,Policko 4 7,Policko 4 8]),(K (Policko 3 1),[Policko 2 2,Policko 3 2,Policko 4 2,Policko 4 1,
-- Policko 2 1])],(K (Policko 6 3),[Policko 5 2,Policko 7 2,Policko 7 3])))
--
matDaK :: ([(Pozicia, [Policko])],(Pozicia, [Policko])) -> (Pozicia,([(Pozicia, [Policko])],(Pozicia, [Policko])))
matDaK ([dama@(D (dampoz), _),kral@(K (_),_)],ckral) =
  if (jeSach novakonstelacia) == True
  then if (jeMat novakonstelacia) == True
       then (fst novapoz, novakonstelacia)
       else matDaK ([(D (dampoz), zvysok),kral], ckral)
  else if (stvorec kandidat kraj) > (stvorec dampoz kraj)
       then matDaK ([kral, dama], ckral)
       else if (stvorec kandidat kraj) /= (stvorec dampoz kraj)
            then (fst novapoz, novakonstelacia)
            else matDaK ([kral, dama], ckral)         
  where (kandidat:zvysok) = (snd (zoradPolicka [dama, kral] ckral))
        kraj = zatlacovaciKraj (polePozicie ckral) dama
        novapoz = poziciaNaPoziciu dama kandidat
        novakonstelacia = napadnutePoliaFigur ([novapoz, kral],ckral)

-- >>> matDaK (napadnutePoliaFigur ([polickaPreFigurku (K (Policko 2 5)), polickaPreFigurku (D (Policko 8 2))], (polickaPreFigurku (K (Policko 4 1)))))
-- (K (Policko 3 4),([(D (Policko 8 2),[Policko 7 1,Policko 3 7,Policko 4 6,Policko 5 5,Policko 6 4,Policko 7 3,Policko 1 2,Policko 2 2,Policko 3 2,
-- Policko 4 2,Policko 5 2,Policko 6 2,Policko 7 2,Policko 8 1,Policko 8 3,Policko 8 4,Policko 8 5,Policko 8 6,Policko 8 7,Policko 8 8]),
-- (K (Policko 3 4),[Policko 2 3,Policko 2 4,Policko 2 5,Policko 4 3,Policko 4 4,Policko 4 5,Policko 3 3,Policko 3 5])],
-- (K (Policko 4 1),[Policko 5 1,Policko 3 1])))
--
matDaK ([kral@(K (kpoz),_), dama@(D (_), _)],ckral) =
  if ((((horizvzd <= 2 && vertvzd <= 1) || (horizvzd <= 1 && vertvzd <= 2)) && (vzdialenostHoriz (polePozicie ckral) kraj <= 1 || vzdialenostVert (polePozicie ckral) kraj <= 1)))
  then matDaK ([dama, kral], ckral)
  else if (stvorec (polePozicie dama) kraj == 8)
       then (fst novapoz, novakonstelacia)
       else (fst novapoz, nazadnadamu)
  where (kandidat:_) = (snd (zoradPolicka [kral, dama] ckral))
        kraj = zatlacovaciKraj (polePozicie ckral) dama
        novapoz = poziciaNaPoziciu kral kandidat
        novakonstelacia = napadnutePoliaFigur ([novapoz, dama],ckral)
        nazadnadamu = napadnutePoliaFigur ([dama, novapoz],ckral)
        horizvzd = (vzdialenostHoriz kpoz (polePozicie ckral))
        vertvzd = (vzdialenostVert kpoz (polePozicie ckral))

matDaK _ = undefined

-- >>> zbierajTahy (napadnutePoliaFigur ([(D (Policko 7 5), []), (K (Policko 1 4), [])], (K (Policko 8 1),[])))
-- [Dg4     Kh2,Kb3     Kh1,Kc2     Kh2,Kd1     Kh1,Ke1     Kh2,Kf1     Kh1,Dg1#]
--
zbierajTahy :: ([(Pozicia, [Policko])], (Pozicia, [Policko])) -> [Tah]
zbierajTahy (biely, cierny) = 
  if (jeMat (snd tahbiely)) == True
  then [TahZaver (fst tahbiely)]
  else ((Tah (fst tahbiely) jesach (fst (snd tahcierny))):(zbierajTahy' tahcierny smer))
 where tahbiely = if (jeDamaDoma biely) == True
                  then matDaKZac (novybiely, novycierny)
                  else matVaV (novybiely, novycierny) smer
       tahcierny = (utekKrala (snd tahbiely))
       novybiely = sortBy (\(a,_) (b,_) -> compare b a) (fst spravnakonstelacia)
       novycierny = snd spravnakonstelacia
       spravnakonstelacia = napadnutePoliaFigur (biely, cierny)
       jesach = jeSach (snd tahbiely)
       bielykral = head [(K (a),b)|(K (a),b) <- biely]
       smer = if (vzdialenostVert (polePozicie bielykral) (polePozicie cierny)) >= 2
              then Riadok
              else Stlpec

zbierajTahy' :: ([(Pozicia, [Policko])], (Pozicia, [Policko])) -> Smer -> [Tah]
zbierajTahy' (biely, cierny) smer = 
  if (jeMat (snd prvytah)) == True
  then [TahZaver (fst prvytah)]
  else ((Tah tahbiely jesach (fst ciernyZahral)):(zbierajTahy' novyStav smer))
  where prvytah = if (jeDamaDoma biely) == True
                  then matDaK (biely, ckral)
                  else matVaV (biely, ckral) smer
        tahbiely = fst prvytah
        druhytah = (utekKrala ((snd prvytah)))
        ciernyZahral = snd druhytah
        novyStav = druhytah
        ckral = cierny
        jesach = jeSach (snd prvytah)

-- >>> jeDamaDoma [(K (Policko 1 1),[]),(K (Policko 1 1),[]),(D (Policko 1 1),[])]
-- True
--
jeDamaDoma :: [(Pozicia, [Policko])] -> Bool
jeDamaDoma [] = False
jeDamaDoma ((D (_),_):_) = True
jeDamaDoma (_:figurky) = jeDamaDoma figurky


-- >>> utekKrala (napadnutePoliaFigur ([(K (Policko 5 2),[]), (D (Policko 7 4), [])], (K (Policko 8 1),[])))
-- ([(K (Policko 5 2),[Policko 4 1,Policko 4 2,Policko 4 3,Policko 6 1,Policko 6 2,Policko 6 3,Policko 5 1,Policko 5 3]),
-- (D (Policko 7 4),[Policko 6 3,Policko 8 5,Policko 3 8,Policko 4 7,Policko 5 6,Policko 6 5,Policko 8 3,Policko 1 4,Policko 2 4,
-- Policko 3 4,Policko 4 4,Policko 5 4,Policko 6 4,Policko 8 4,Policko 7 1,Policko 7 2,Policko 7 3,Policko 7 5,Policko 7 6,Policko 7 7,Policko 7 8])],
-- (K (Policko 8 2),[Policko 8 1]))
--
utekKrala :: ([(Pozicia, [Policko])], (Pozicia,[Policko])) -> ([(Pozicia, [Policko])],(Pozicia,[Policko]))
utekKrala (super, kral)
  | kamskralom == [] = (super,(fst kral, []))
  | otherwise = novepolicka
 where kamskralom = snd kral
       novepolicka = napadnutePoliaFigur ([s | s <- super, (polePozicie s) /= prvytip], (poziciaNaPoziciu kral prvytip))
       prvytip = (head (sortBy (\x y -> compare (vzdialenost (Policko 4 4) x) (vzdialenost (Policko 4 4) y)) kamskralom))

-- pomocna funkcia pre funkciu vyssie, filtruje nemozne policka
utekKrala' :: (Pozicia,[Policko]) -> [(Pozicia, [Policko])] -> [Policko]
utekKrala' (_, policka) [] = policka
utekKrala' (poz, policka) ((_, s):super) = utekKrala' (poz,novapoz) super
 where novapoz = [p | p <- policka, not (p `elem` s)]


matVaV :: ([(Pozicia,[Policko])],(Pozicia, [Policko])) -> Smer -> (Pozicia,([(Pozicia,[Policko])],(Pozicia, [Policko])))
matVaV figurky Riadok = sekajRiadky figurky 
matVaV figurky Stlpec = sekajStlpce figurky 

-- >>> sekajRiadky (napadnutePoliaFigur ([polickaPreFigurku x | x <- [V (Policko 2 7), V (Policko 8 6), K (Policko 4 4)]], (polickaPreFigurku (K (Policko 7 8)))))       
-- (V (Policko 1 6),([(V (Policko 2 7),[Policko 1 7,Policko 3 7,Policko 4 7,Policko 5 7,Policko 6 7,Policko 7 7,Policko 8 7,
-- Policko 2 1,Policko 2 2,Policko 2 3,Policko 2 4,Policko 2 5,Policko 2 6,Policko 2 8]),(V (Policko 1 6),[Policko 2 6,Policko 3 6,Policko 4 6,
-- Policko 5 6,Policko 6 6,Policko 7 6,Policko 8 6,Policko 1 1,Policko 1 2,Policko 1 3,Policko 1 4,Policko 1 5,Policko 1 7,Policko 1 8]),
-- (K (Policko 4 4),[Policko 3 3,Policko 3 4,Policko 3 5,Policko 5 3,Policko 5 4,Policko 5 5,Policko 4 3,Policko 4 5])],
-- (K (Policko 7 8),[Policko 8 8,Policko 6 8])))
--
sekajRiadky :: ([(Pozicia,[Policko])],(Pozicia, [Policko])) -> (Pozicia,([(Pozicia,[Policko])],(Pozicia, [Policko])))
sekajRiadky figurky@([veza1@(V (Policko _ v1y),v1policka), veza2@(V (Policko _ v2y), v2policka), kral@(K (Policko _ ky),_)],ckral@(K (Policko _ cky),_))
 | v1y - smer == cky = if (bezpecnePolicko veza1 [veza1,veza2,kral] ckral ) == True
                       then (fst novapoz22, napadnutePoliaFigur ([veza1, novapoz22, kral], ckral))
                       else (fst novapoz21, napadnutePoliaFigur ([novapoz21, veza2, kral], ckral))
 | v2y - smer == cky = if (bezpecnePolicko veza2 [veza2,veza1,kral] ckral ) == True
                       then (fst novapoz11, napadnutePoliaFigur ([novapoz11, veza2, kral], ckral))
                       else (fst novapoz12, napadnutePoliaFigur ([veza1, novapoz12, kral], ckral))
 | otherwise = podrezKralaRiadok figurky
 where smer = signum (ky - cky)
       sachv2 = ([(Policko x y)| (Policko x y) <- v2policka, y == cky, not ((Policko x y) `elem` (snd (polickaPreFigurku (fst ckral))))])
       novapoz22 = if sachv2 /= []
                  then poziciaNaPoziciu veza2 (head sachv2)
                  else riadokHet [veza2, veza1]
       novapoz21 = riadokHet [veza1, veza2]
       sachv1 = ([(Policko x y)| (Policko x y) <- v1policka, y == cky, not ((Policko x y) `elem` (snd (polickaPreFigurku (fst ckral))))])
       novapoz11 = if sachv1 /= []
                  then poziciaNaPoziciu veza1 (head sachv1)
                  else riadokHet [veza1, veza2]
       novapoz12 = riadokHet [veza2, veza1]
      
sekajRiadky _ = undefined

podrezKralaRiadok :: ([(Pozicia, [Policko])], (Pozicia, [Policko])) -> (Pozicia, ([(Pozicia, [Policko])], (Pozicia, [Policko])))
podrezKralaRiadok ([veza1@(V (_),v1policka), veza2@(V (_), v2policka), kral@(K (Policko _ ky),_)], ckral@(K ckpole@(Policko _ cky), _)) =
 if (bezpecnePolicko (head novapoz) novapoz ckral) == True
 then (fst (head novapoz), napadnutePoliaFigur (novapoz, ckral))  
 else (fst utek, napadnutePoliaFigur ([head novapoz, utek, last novapoz], ckral))
 where smer = signum (ky - cky)
       v1kandidat = head [(Policko a b) | (Policko a b) <- v1policka, b == (cky + smer)]
       v2kandidat = head [(Policko a b) | (Policko a b) <- v2policka, b == (cky + smer)]
       novapoz = if (vzdialenostHoriz v1kandidat ckpole) <= (vzdialenostHoriz v2kandidat ckpole)
                 then [(poziciaNaPoziciu veza2 v2kandidat), veza1, kral]
                 else [(poziciaNaPoziciu veza1 v1kandidat), veza2, kral]
       utek = riadokHet (reverse (take 2 novapoz))
podrezKralaRiadok _ = undefined


-- >>> sekajStlpce (napadnutePoliaFigur ([polickaPreFigurku x | x <- [V (Policko 6 1), V (Policko 7 7), K (Policko 8 1)]], (polickaPreFigurku (K (Policko 3 3)))))
-- (V (Policko 4 7),([(V (Policko 4 7),[Policko 1 7,Policko 2 7,Policko 3 7,Policko 5 7,Policko 6 7,Policko 7 7,Policko 8 7,
-- Policko 4 1,Policko 4 2,Policko 4 3,Policko 4 4,Policko 4 5,Policko 4 6,Policko 4 8]),
-- (V (Policko 6 1),[Policko 1 1,Policko 2 1,Policko 3 1,Policko 4 1,Policko 5 1,Policko 7 1,Policko 6 2,Policko 6 3,Policko 6 4,Policko 6 5,
-- Policko 6 6,Policko 6 7,Policko 6 8]),
-- (K (Policko 8 1),[Policko 7 1,Policko 7 2,Policko 8 2])],
-- (K (Policko 3 3),[Policko 2 2,Policko 2 3,Policko 2 4,Policko 3 2,Policko 3 4])))
--
sekajStlpce :: ([(Pozicia,[Policko])],(Pozicia, [Policko])) -> (Pozicia,([(Pozicia,[Policko])],(Pozicia, [Policko])))
sekajStlpce figurky@([veza1@(V (Policko v1x _),v1policka), veza2@(V (Policko v2x _), v2policka), kral@(K (Policko kx _),_)],ckral@(K (Policko ckx _),_))
 | v1x - smer == ckx = if (bezpecnePolicko veza1 [veza1,veza2,kral] ckral ) == True
                       then (fst novapoz22, napadnutePoliaFigur ([veza1, novapoz22, kral], ckral))
                       else (fst novapoz21, napadnutePoliaFigur ([novapoz21, veza2, kral], ckral))
 | v2x - smer == ckx = if (bezpecnePolicko veza2 [veza2,veza1,kral] ckral ) == True
                       then (fst novapoz11, napadnutePoliaFigur ([novapoz11, veza2, kral], ckral))
                       else (fst novapoz12, napadnutePoliaFigur ([veza1, novapoz12, kral], ckral))
 | otherwise = podrezKralaStlpec figurky
 where smer = signum (kx - ckx)
       sachv2 = ([(Policko x y)| (Policko x y) <- v2policka, x == ckx, not ((Policko x y) `elem` (snd (polickaPreFigurku (fst ckral))))])
       novapoz22 = if sachv2 /= []
                  then poziciaNaPoziciu veza2 (head sachv2)
                  else stlpecHet [veza2, veza1]
       novapoz21 = stlpecHet [veza1, veza2]
       sachv1 = ([(Policko x y)| (Policko x y) <- v1policka, x == ckx, not ((Policko x y) `elem` (snd (polickaPreFigurku (fst ckral))))])
       novapoz11 = if sachv1 /= []
                  then poziciaNaPoziciu veza1 (head sachv1)
                  else stlpecHet [veza1, veza2]
       novapoz12 = stlpecHet [veza2, veza1]
sekajStlpce _ = undefined

podrezKralaStlpec :: ([(Pozicia, [Policko])], (Pozicia, [Policko])) -> (Pozicia, ([(Pozicia, [Policko])], (Pozicia, [Policko])))
podrezKralaStlpec ([veza1@(V (_),v1policka), veza2@(V (_), v2policka), kral@(K (Policko kx _),_)], ckral@(K ckpole@(Policko ckx _), _)) =
 if (bezpecnePolicko (head novapoz) novapoz ckral) == True
 then (fst (head novapoz), napadnutePoliaFigur (novapoz, ckral))  
 else (fst utek, napadnutePoliaFigur ([head novapoz, utek, last novapoz], ckral))
 where smer = signum (kx - ckx)
       v1kandidat = head [(Policko a b) | (Policko a b) <- v1policka, a == (ckx + smer)]
       v2kandidat = head [(Policko a b) | (Policko a b) <- v2policka, a == (ckx + smer)]
       novapoz = if (vzdialenostVert v1kandidat ckpole) <= (vzdialenostVert v2kandidat ckpole)
                 then [(poziciaNaPoziciu veza2 v2kandidat), veza1, kral]
                 else [(poziciaNaPoziciu veza1 v1kandidat), veza2, kral]
       utek = stlpecHet (reverse (take 2 novapoz))
podrezKralaStlpec _ = undefined



-- >>> riadokHet [polickaPreFigurku (prelozStringNaPoziciu x) | x <- ["Va3", "Vg7"]]
-- (V (Policko 8 3),[Policko 1 3,Policko 2 3,Policko 3 3,Policko 4 3,Policko 5 3,Policko 6 3,Policko 7 3,
-- Policko 8 1,Policko 8 2,Policko 8 4,Policko 8 5,Policko 8 6,Policko 8 7,Policko 8 8])
--
riadokHet :: [(Pozicia, [Policko])] -> (Pozicia, [Policko])
riadokHet ((veza@(V (Policko x y), vpolicka):ostatneFigurky))
 | x <= 4 = poziciaNaPoziciu veza (last policka)
 | otherwise = poziciaNaPoziciu veza (head policka)
 where policka = sortBy (\(Policko aa _) (Policko ba _) -> compare aa ba) [(Policko a b) | (Policko a b) <- vpolicka, b == y, a /= v]
       (Policko v _) = polePozicie (head ostatneFigurky)
riadokHet _ = undefined

stlpecHet :: [(Pozicia, [Policko])] -> (Pozicia, [Policko])
stlpecHet ((veza@(V (Policko x y), vpolicka):ostatneFigurky))
 | y <= 4 = poziciaNaPoziciu veza (last policka)
 | otherwise = poziciaNaPoziciu veza (head policka)
 where policka = sortBy (\(Policko _ aa) (Policko _ ba) -> compare aa ba) [(Policko a b) | (Policko a b) <- vpolicka, a == x, b /= w]
       (Policko _ w) = polePozicie (head ostatneFigurky)
stlpecHet _ = undefined
 

-- >>> mozebiely (polickaPreFigurku (K (Policko 4 7))) (polickaPreFigurku (K (Policko 6 7)))
-- (K (Policko 4 7),[Policko 3 6,Policko 3 7,Policko 3 8,Policko 4 6,Policko 4 8])
--
mozebiely :: (Pozicia, [Policko]) -> (Pozicia, [Policko]) -> (Pozicia, [Policko])
mozebiely (K (x), policka) ckral = (K (x), [f | f <- policka, not (f `elem` (snd (polickaPreFigurku (fst ckral))))])
mozebiely biely _ = biely

-- >>> napadnutePoliaFigur ([polickaPreFigurku x | x <- [K (Policko 6 1), D (Policko 7 1)]], (polickaPreFigurku (K (Policko 8 1))))
-- ([(K (Policko 6 1),[Policko 5 2,Policko 6 2,Policko 5 1]),
-- (D (Policko 7 1),[Policko 8 2,Policko 1 7,Policko 2 6,Policko 3 5,Policko 4 4,Policko 5 3,Policko 6 2,
-- Policko 8 1,Policko 7 2,Policko 7 3,Policko 7 4,Policko 7 5,Policko 7 6,Policko 7 7,Policko 7 8])],
-- (K (Policko 8 1),[]))
--
napadnutePoliaFigur :: ([(Pozicia, [Policko])], (Pozicia, [Policko])) -> ([(Pozicia, [Policko])], (Pozicia, [Policko]))
napadnutePoliaFigur (bpozicie, ckral) = (outb, (fst ckral, moznepolia))
 where noveb = (napadnutePoliaFigur' [polickaPreFigurku (fst p) | p <- bpozicie] [polePozicie x | x <- bpozicie])
       preckrala = napadnutePoliaFigur'' [polickaPreFigurku (fst p) | p <- bpozicie] [polePozicie x | x <- bpozicie]
       moznepolia = (utekKrala'(polickaPreFigurku (fst ckral)) preckrala)
       outb = [mozebiely f ckral| f <- noveb]

napadnutePoliaFigur' :: [(Pozicia, [Policko])] -> [Policko] -> [(Pozicia,[Policko])]
napadnutePoliaFigur' [] _ = []
napadnutePoliaFigur' (p:pozicie) polickafigur = (((pozicia, [x | x <- zoznam, not (x `elem` polickafigur)])):(napadnutePoliaFigur' pozicie polickafigur))
 where (pozicia, zoznam) = odfiltrujBlokPolia p [x| x <- polickafigur, x /= (polePozicie p)]

napadnutePoliaFigur'' :: [(Pozicia, [Policko])] -> [Policko] -> [(Pozicia,[Policko])]
napadnutePoliaFigur'' [] _ = []
napadnutePoliaFigur'' (p:pozicie) polickafigur = (((pozicia, [x | x <- zoznam])):(napadnutePoliaFigur'' pozicie polickafigur))
 where 
   (pozicia, zoznam) = odfiltrujBlokPolia p [x| x <- polickafigur, x /= (polePozicie p)]
       

-- testy

-- dama medzi kralmi
-- >>> dajMat ["Da2", "Ka1"] "Ke5"
-- Dc4     Kd6
-- Db5     Ke6
-- Dc5     Kd7
-- Db6     Ke7
-- Dc6     Kd8
-- Db7     Ke8
-- Dc7     Kf8
-- Dd7     Kg8
-- De7     Kh8
-- Kb2     Kg8
-- Kc3     Kh8
-- Kd4     Kg8
-- Ke5     Kh8
-- Kf6     Kg8
-- Dg7#

-- dama nie je medzi kralmi
-- >>> dajMat ["Da1", "Kc2"] "Kf4"
-- Da3     Ke4
-- Dc3     Kd5
-- Db4     Ke5
-- Dc4     Kd6
-- Db5     Ke6
-- Dc5     Kd7
-- Db6     Ke7
-- Dc6     Kd8
-- Db7     Ke8
-- Dc7     Kf8
-- Dd7     Kg8
-- De7     Kh8
-- Kd3     Kg8
-- Ke4     Kh8
-- Kf5     Kg8
-- Kg6     Kh8
-- Dh7#
--

-- >>> dajMat ["Df7", "Ke2"] "Kg1"
-- Df3     Kh2
-- Dg4     Kh1
-- Kf1     Kh2
-- Dh4#
--

-- >>> dajMat ["Db7", "Ke6"] "Kd8"
-- Db8#
--


-- veze matujuce po riadkoch
-- >>> dajMat ["Vb1", "Vg2", "Kh3"] "Kd6"
-- Vg5     Kc6
-- Vh1     Kd6
-- Va1     Kc6
-- Va6+    Kd7
-- Vg7+    Kd8
-- Va8#
--

-- veze matujuce po stlpcoch
-- >>> dajMat ["Ve7", "Vb4", "Ka3"] "Kf3"
-- Vb8     Kf4
-- Vf8+    Kg4
-- Vg7+    Kh4
-- Vh8#
--

-- veza je napadnuta
-- >>> dajMat ["Va3", "Vg2", "Kc4"] "Kh1"
-- Vb2     Kg1
-- Va1#
--

-- veze v jednom stlpci
-- >>> dajMat ["Ve7", "Ve4", "Ka3"] "Kf3"
-- Ve6     Kf4
-- Vf6+    Kg4
-- Vg7+    Kh4
-- Vh6#
--
 
-- veze v jednom riadku 
-- >>> dajMat ["Ve2", "Va2", "Ka3"] "Kg1"
-- Va1#
--
