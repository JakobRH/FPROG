--1

generiere_fak_strom :: [Integer]
generiere_fak_strom = [fac(n)| n <- [0..]]

fac :: Integer -> Integer
fac n
    | n == 0 = 1
    | n > 0  = n * fac (n-1)
    | True   = n


--2

type IR_plus     = Double   -- Nur Werte echt groesser als null
type Stelle      = Double
type Genauigkeit = IR_plus
type Approx_Wert = Double
type Strom       = [Double]

approximiere_exp :: Stelle -> Genauigkeit -> Approx_Wert
approximiere_exp x epsilon = selektiere epsilon (generiere_exp_strom x)

generiere_exp_strom :: Stelle -> Strom
generiere_exp_strom x = [exp_help x n| n <- [0..]]

selektiere :: Genauigkeit -> Strom -> Approx_Wert
selektiere g s = selektiere_help g s 0 1

selektiere_help :: Genauigkeit -> Strom -> Int -> Int -> Approx_Wert
selektiere_help g s i1 i2 = if (s !! i2) - (s !! i1) <= g then (s !! i2) else selektiere_help g s (i1+1) (i2+1) 

exp_help :: Double -> Integer -> Double
exp_help x y = 1 + sum ([ ((x ^ n) / fromIntegral (fac n)) | n <- [1..y]])

--3
type Woerterstrom = [String]
alphabet = ["a","b","c"]

generiere_woerter :: Woerterstrom
generiere_woerter = [""] ++ alphabet ++ generiere_woerter_help alphabet

generiere_woerter_help :: [String] -> Woerterstrom
generiere_woerter_help x = y ++ generiere_woerter_help y where y = kreuzprodukt x alphabet

kreuzprodukt :: [String] -> [String] -> [String]
kreuzprodukt x y = [(a++b)|a<-x,b<-y] 


filtere_palindrome :: Woerterstrom -> Woerterstrom
filtere_palindrome x = [n | n <- x, (reverse n) == n] 

--4

type Wort        = String
type Woerterbuch = [Wort] -- nur Werte endliche Laenge; keine Stroeme.
type Wortleiter  = [Wort]


ist_aufsteigende_leiterstufe :: Wort -> Wort -> Bool
ist_aufsteigende_leiterstufe x y = if y < x then False else ist_leiterstufe x y

ist_leiterstufe :: Wort -> Wort -> Bool
ist_leiterstufe x y
    | x == (init y) = True
    | (tail x) == y = True
    | length x == length y = if length [n | n <- [0..((length y)-1)], (y !! n) == (x !! n)] >= ((length y)-1) then True else False
    | otherwise = False


ist_aufsteigende_wortleiter :: [Wort] -> Bool
ist_aufsteigende_wortleiter x = if length [n | n <- [0..((length x)-2)], ist_aufsteigende_leiterstufe (x !! n) (x !! (n+1))] == ((length x)-1) then True else False

