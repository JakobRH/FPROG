data Arith_Variable = A1 | A2 | A3 | A4 | A5 | A6 deriving (Eq,Show)
data Log_Variable   = L1 | L2 | L3 | L4 | L5 | L6 deriving (Eq,Show)

data Arith_Ausdruck = AK Int               -- Arithmetische Konstante
    | AV Arith_Variable  -- Arithmetische Variable
    | Plus Arith_Ausdruck Arith_Ausdruck  -- Addition
    | Minus Arith_Ausdruck Arith_Ausdruck -- Subtraktion
    | Mal Arith_Ausdruck Arith_Ausdruck   --Multiplikation
    deriving (Eq,Show)

data Log_Ausdruck = LK Bool                           -- Logische Konstante
    | LV Log_Variable                 -- Logische Variable
    | Nicht Log_Ausdruck              -- Logische Negation
    | Und Log_Ausdruck Log_Ausdruck   -- Logische Konjunktion
    | Oder Log_Ausdruck Log_Ausdruck  -- Logische Disjunktion
    | Gleich Arith_Ausdruck Arith_Ausdruck  -- Wertgleichheit-- arith. Ausdruecke
    | Kleiner Arith_Ausdruck Arith_Ausdruck -- Linker Ausdruck-- echt wertkleiner-- als rechter-- Ausdruck
    deriving (Eq,Show)

type Arith_Variablenbelegung = Arith_Variable -> Int -- Total definierte Abb.
type Log_Variablenbelegung   = Log_Variable -> Bool   -- Total definierte Abb.
type Variablenbelegung       = (Arith_Variablenbelegung,Log_Variablenbelegung) 


links :: (Either a b) -> a
links (Left x) = x

rechts :: (Either a b) -> b
rechts (Right y) = y

class Evaluierbar a where
    evaluiere :: a -> Variablenbelegung -> Either Int Bool

instance Evaluierbar Arith_Ausdruck where
    evaluiere x y = Left(evaluiere_help_arith x y)

evaluiere_help_arith :: Arith_Ausdruck -> Variablenbelegung -> Int
evaluiere_help_arith (AK x) y =  x
evaluiere_help_arith (AV x) (y, z) =  y x
evaluiere_help_arith (Plus x1 x2) y = evaluiere_help_arith x1 y + evaluiere_help_arith x2 y
evaluiere_help_arith (Minus x1 x2) y = evaluiere_help_arith x1 y - evaluiere_help_arith x2 y
evaluiere_help_arith (Mal x1 x2) y = evaluiere_help_arith x1 y * evaluiere_help_arith x2 y


instance Evaluierbar Log_Ausdruck where 
    evaluiere x y = Right(evaluiere_help_log x y)

evaluiere_help_log :: Log_Ausdruck -> Variablenbelegung -> Bool
evaluiere_help_log (LK x) y = x
evaluiere_help_log (LV x) (y, z) = z x
evaluiere_help_log (Nicht x) y = not (evaluiere_help_log x y)
evaluiere_help_log (Und x1 x2) y = evaluiere_help_log x1 y && evaluiere_help_log x2 y
evaluiere_help_log (Oder x1 x2) y = evaluiere_help_log x1 y || evaluiere_help_log x2 y
evaluiere_help_log (Gleich x1 x2) y = evaluiere_help_arith x1 y == evaluiere_help_arith x2 y
evaluiere_help_log (Kleiner x1 x2) y = evaluiere_help_arith x1 y < evaluiere_help_arith x2 y 








type Adresse = Int
type Sprungadresse = Adresse
data Anweisung = AZ Arith_Variable Arith_Ausdruck -- Wertzuweisung an-- arithmetische Variable
                 | LZ Log_Variable Log_Ausdruck   -- Wertzuweisung an-- logische Variable
                 | FU Log_Ausdruck Sprungadresse Sprungadresse -- Fallunter--- scheidung
                 | BS Log_Ausdruck Sprungadresse  -- Bedingter Sprung
                 | US Sprungadresse               -- Unbedingter Sprung
                 | MP Adresse Anweisung           -- Selbstmodifikation des Programms


type Zustand         = Variablenbelegung
type Anfangszustand  = Zustand
type Endzustand      = Zustand
type Zwischenzustand = Zustand
type Programm        = [Anweisung]
type EPS             = Programm


interpretiere_1 :: EPS -> Anfangszustand -> Endzustand
interpretiere_1 x y = ip_1_help1 x y 0

ip_1_help1 :: [Anweisung] -> Zustand -> Int -> Endzustand
ip_1_help1 x (a,b) n  
    | n >= length x = (a,b)
    | otherwise = ip_1_help1 (ip_1_help4 x  (x !! n) (a,b) n) (ip_1_help2 (x !! n) (a,b) n) (ip_1_help3 x (x !! n) (a,b) n) 

--liefert einen neuen Zustand falls es sich bei der Anweisung um ein Wertzuweisung handelt
ip_1_help2 :: Anweisung -> Zustand -> Int -> Endzustand
ip_1_help2 (AZ a1 a2) (a,b) n = (z, b) where z = (\ av -> if av == a1 then c else a av) ::Arith_Variablenbelegung 
                                             c = (links(evaluiere a2 (a,b)))
ip_1_help2 (LZ l1 l2) (a,b) n = (a, z) where z = (\ lv -> l1) :: Log_Variablenbelegung
                                             l1 = (rechts(evaluiere l2 (a,b)))
ip_1_help2 x y z = y

--liefert die nächste auszuführende Adresse im gegeben Programm 
ip_1_help3 :: [Anweisung] -> Anweisung -> Zustand -> Int -> Int
ip_1_help3 x (FU l1 s1 s2) (a,b) n = if rechts(evaluiere l1 (a,b)) == True then s1 else s2
ip_1_help3 x (BS l1 s1) (a,b) n = if rechts(evaluiere l1 (a,b)) == True then s1 else n+1
ip_1_help3 x (US s1) (a,b) n = s1
ip_1_help3 x (MP a1 a2) (a,b) n = if a1 >= 0 && a1 <= length x then a1 else n+1
ip_1_help3 _ _ _ n = n+1

--fügt eine neue Anweisung in das Programm falls es sich um eine Modifikationsanweisung handelt
ip_1_help4 :: [Anweisung] -> Anweisung -> Zustand -> Int -> [Anweisung]
ip_1_help4 x (MP a1 a2) (a,b) n = if a1 >= 0 && a1 <= ((length x)-1) then replace a1 a2 x else if a1 == (length x) then (x ++ [a2]) else x
ip_1_help4 x _ _ _ = x

--ersetzt ein Element in der gegeben Liste
replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace n newX (x:xs)
    | n == 0 = newX:xs
    | otherwise = x:replace (n-1) newX xs


interpretiere_2 :: EPS -> Anfangszustand -> [Zwischenzustand]
interpretiere_2 x y = ip_2_help1 x [y] y 0

ip_2_help1 :: [Anweisung] -> [Zustand] -> Zustand -> Int -> [Zustand]
ip_2_help1 x li (a,b) n  
    | n >= length x = li
    | otherwise = li ++ ip_2_help1 (ip_1_help4 x (x !! n) (a,b) n) li1 (ip_1_help2 (x !! n) (a,b) n) (ip_1_help3 x (x !! n) (a,b) n) 
     where li1 = add_to_ip2 (x !! n) (a,b) n

--fügt eine neu Zuweisung in die Liste falls es sich um eine Wertzuweisung handelt
add_to_ip2 :: Anweisung -> Zustand -> Int -> [Zustand]
add_to_ip2 (AZ a1 a2) (a,b) n = [(z, b)] where z = (\ av -> if av == a1 then c else a av) ::Arith_Variablenbelegung 
                                               c = (links(evaluiere a2 (a,b)))
add_to_ip2 (LZ l1 l2) (a,b) n = [(a, z)] where z = (\ lv -> l1) :: Log_Variablenbelegung
                                               l1 = (rechts(evaluiere l2 (a,b)))
add_to_ip2 x y z = []

--BEISPIELE

fib :: Int -> Int
fib n
    | n == 0 = 0
    | n == 1 = 1
    | n > 0  = fib (n-2) + fib (n-1)
    | True   = n

fac :: Int -> Int
fac n
    | n == 0 = 1
    | n > 0  = n * fac (n-1)
    | True   = n



--2

gib_aus_arith_Varbel :: Arith_Variablenbelegung -> [(Arith_Variable,Int)]
gib_aus_arith_Varbel avb = [(A1, avb A1), (A2 ,avb A2), (A3 ,avb A3), (A4 ,avb A4), (A5 ,avb A5), (A6 ,avb A6)]

gib_aus_log_Varbel :: Log_Variablenbelegung -> [(Log_Variable,Bool)]
gib_aus_log_Varbel lvb = [(L1, lvb L1),(L2, lvb L2),(L3, lvb L3),(L4, lvb L4),(L5, lvb L5),(L6, lvb L6)]


gib_aus_Zustand :: Zustand -> ([(Arith_Variable,Int)],[(Log_Variable,Bool)])
gib_aus_Zustand (a,l) = (gib_aus_arith_Varbel a, gib_aus_log_Varbel l)


--3a
ggt :: EPS
ggt = [FU (Gleich (AV A2) (AK 0)) 6 1,
       FU (Kleiner (AV A2) (AV A1)) 2 4,
       AZ A1 (Minus (AV A1) (AV A2)),
       US 0, 
       AZ A2 (Minus (AV A2) (AV A1)),
       US 0, 
       AZ A3 (AV A1)]


fibo :: EPS
fibo = [FU (Kleiner (AV A1) (AK 0)) 1 4,
        AZ A1 (Minus (AV A1) (Mal (AV A1) (AK 2))),
        LZ L1 (LK False),
        US 5,
        LZ L1 (LK True),
        AZ A6 (AK 0),
        AZ A5 (AK 1),
        AZ A4 (AK 1),
        AZ A3 (AK 0),
        FU (Kleiner (AV A3) (AV A1)) 10 15,
        AZ A6 (AV A5),
        AZ A5 (AV A4),
        AZ A4 (Plus (AV A6) (AV A5)),
        AZ A3 (Plus (AV A3) (AK 1)),
        US 9,
        AZ A6 (AV A6)
        ]







--3b
azst1 :: Anfangszustand
azst1 = ((\ av -> if av == A1 then 24 else if av == A2 then 60 else 0), (\ lv -> True));


azst2 :: Anfangszustand
azst2 = (azst2_avb, azst2_lvb)

azst2_avb = (\ av -> case av of A1 -> 18
                                A2 -> 45
                                A3 -> 3
                                A4 -> 4
                                A5 -> 5
                                A6 -> 6 )::Arith_Variablenbelegung
        
azst2_lvb = (\ lv -> case lv of L1 -> False
                                L2 -> True
                                L3 -> False
                                L4 -> True
                                L5 -> False
                                L6 -> True ):: Log_Variablenbelegung

--3c
generiere :: [Int] -> [Bool] -> Zustand
generiere as bs = (arithvb, logvb) where arithvb = (\ av -> case av of A1 -> if length as >= 1 then as !! 0 else 0
                                                                       A2 -> if length as >= 2 then as !! 1 else 0
                                                                       A3 -> if length as >= 3 then as !! 2 else 0
                                                                       A4 -> if length as >= 4 then as !! 3 else 0
                                                                       A5 -> if length as >= 5 then as !! 4 else 0
                                                                       A6 -> if length as >= 6 then as !! 5 else 0)
                                         logvb = (\ lv -> case lv of L1 -> if length bs >= 1 then bs !! 0 else False
                                                                     L2 -> if length bs >= 2 then bs !! 1 else False
                                                                     L3 -> if length bs >= 3 then bs !! 2 else False
                                                                     L4 -> if length bs >= 4 then bs !! 3 else False
                                                                     L5 -> if length bs >= 5 then bs !! 4 else False
                                                                     L6 -> if length bs >= 6 then bs !! 5 else False)



