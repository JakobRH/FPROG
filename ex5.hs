type Nat0       = Int
type Nat1       = Int
data Wochentag  = Mo | Di | Mi | Do | Fr | Sa | So deriving (Eq,Show)
type Starttag   = Wochentag
type Zeitraum_in_Tagen            = Nat1
type Streikaufrufabstand_in_Tagen = Nat1
newtype Partei  = P Streikaufrufabstand_in_Tagen deriving Show
type Parteien   = [Partei]
type Streiktage = Nat0
type Anzahl_Parteien = Nat1
type Modellszenario  = (Starttag,Zeitraum_in_Tagen,Parteien)


streiktage :: Modellszenario -> Streiktage
streiktage x = streiktage_h x 1

streiktage_h :: Modellszenario -> Int-> Streiktage
streiktage_h (Fr,x,y) u = if u == (x+1) then 0 else streiktage_h (Sa,x,y) (u+1)
streiktage_h (So,x,y) u = if u == (x+1) then 0 else streiktage_h (Mo,x,y) (u+1)
streiktage_h (z,x,y) u = if u == (x+1) then 0 else (streiktage_help(y,u)) + streiktage_h ((next_tag z), x, y) (u+1)

streiktage_help :: ([Partei], Nat1) -> Nat0
streiktage_help (y, x) = if length[n | n <- y, mod x (get_partei n) == 0] > 0 then 1 else 0

get_partei :: Partei -> Nat1
get_partei (P x) = x

next_tag :: Wochentag -> Wochentag
next_tag Mo = Di
next_tag Di = Mi
next_tag Mi = Do
next_tag Do = Fr
next_tag Fr = Sa
next_tag Sa = So
next_tag So = Mo

superstreiktage :: Modellszenario -> Streiktage
superstreiktage x = superstreiktage_h x 1

superstreiktage_h :: Modellszenario -> Int-> Streiktage
superstreiktage_h (Fr,x,y) u = if u == (x+1) then 0 else superstreiktage_h (Sa,x,y) (u+1)
superstreiktage_h (So,x,y) u = if u == (x+1) then 0 else superstreiktage_h (Mo,x,y) (u+1)
superstreiktage_h (z,x,y) u = if u == (x+1) then 0 else (superstreiktage_help(y,u)) + superstreiktage_h ((next_tag z), x, y) (u+1)

superstreiktage_help :: ([Partei], Nat1) -> Nat0
superstreiktage_help (y, x) = if length[n | n <- y, mod x (get_partei n) == 0] == length y then 1 else 0


grossstreiktage :: Modellszenario -> Anzahl_Parteien -> Streiktage
grossstreiktage x y = grossstreiktage_h x 1 y

grossstreiktage_h :: Modellszenario -> Int-> Nat1 -> Streiktage
grossstreiktage_h (Fr,x,y) u l = if u == (x+1) then 0 else grossstreiktage_h (Sa,x,y) (u+1) l
grossstreiktage_h (So,x,y) u l = if u == (x+1) then 0 else grossstreiktage_h (Mo,x,y) (u+1) l
grossstreiktage_h (z,x,y) u l = if u == (x+1) then 0 else (grossstreiktage_help(y,u,l)) + grossstreiktage_h ((next_tag z), x, y) (u+1) l
    
grossstreiktage_help :: ([Partei], Nat1, Nat1) -> Nat0
grossstreiktage_help (y, x, z) = if length[n | n <- y, mod x (get_partei n) == 0] >= z then 1 else 0


streiktage_am :: Modellszenario -> Wochentag -> Anzahl_Parteien ->Streiktage
streiktage_am x y z = streiktageam_h x 1 z y

streiktageam_h :: Modellszenario -> Int-> Nat1 -> Wochentag -> Streiktage
streiktageam_h (Fr,x,y) u l v = if u == (x+1) then 0 else streiktageam_h (Sa,x,y) (u+1) l v
streiktageam_h (So,x,y) u l v = if u == (x+1) then 0 else streiktageam_h (Mo,x,y) (u+1) l v
streiktageam_h (z,x,y) u l v = if u == (x+1) then 0 else if z == v then (grossstreiktage_help(y,u,l)) + streiktageam_h ((next_tag z), x, y) (u+1) l v else streiktageam_h ((next_tag z), x, y) (u+1) l v
    

wird_gestreikt :: Modellszenario -> Nat1 -> Bool
wird_gestreikt x n = wirdgestreikt_h x 1 n

wirdgestreikt_h :: Modellszenario -> Int -> Nat1 -> Bool
wirdgestreikt_h (Fr,x,y) u n = if u == (x+1) then False else wirdgestreikt_h (Sa,x,y) (u+1) n
wirdgestreikt_h (So,x,y) u n = if u == (x+1) then False else wirdgestreikt_h (Mo,x,y) (u+1) n
wirdgestreikt_h (z,x,y) u n = if u == (x+1) then False else if n == u then (wirdgestreikt_help(y,u)) else wirdgestreikt_h ((next_tag z),x,y) (u+1) n

wirdgestreikt_help :: ([Partei], Nat1) -> Bool
wirdgestreikt_help (y, x) = if length[n | n <- y, mod x (get_partei n) == 0] > 0 then True else False

---------------------------------------------------------------------------------------------------------


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



































