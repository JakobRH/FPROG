type Nat0 = Int
type Nat1 = Int
streiche :: String -> Int -> Char -> String
streiche s i c 
    | i <=0 = s
    | otherwise = streiche1 s 1 i c
streiche1 :: String -> Int -> Int -> Char -> String
streiche1 s i1 i2 c 
    | s == "" = ""
    | head s /= c = [head s] ++ (streiche1 (tail s) i1 i2 c)
    | mod i1 i2 /= 0 || i1 == 0 = [head s] ++ (streiche1 (tail s) (i1+1) i2 c)
    | otherwise = "" ++ (streiche1 (tail s) (i1+1) i2 c)

potenz :: Int -> Int -> Int
potenz a b = a ^ b
-- 2A
ist_umgekehrt_2er_potenz :: Nat0 -> Bool
ist_umgekehrt_2er_potenz x = if x /= 0 then ist_umgekehrt_2er_potenz1(reverse1 x) else False
ist_umgekehrt_2er_potenz1 :: Int -> Bool
ist_umgekehrt_2er_potenz1 x
    | x == 1 = True
    | mod x 2 == 0 = ist_umgekehrt_2er_potenz1(div x 2)
    | otherwise = False

reverse1 :: Int -> Int
reverse1 = read . reverse . show

--3A
groesstes_palindrom_in :: [Nat0] -> Int
groesstes_palindrom_in x = maximum (map is_palindrom(x))

is_palindrom :: Nat0 -> Nat0
is_palindrom x = if x == reverse1 x then x else -1

{- B.1:
a) Die Funktion berechnet mit einem nicht negativen Argument n die Potenz 5^n
   Die Funktion endet mit einem echt negativem Argument in einem Stack-Overflow, da es zu einer endlosen Rekursion führt.
b) calculate_nth_power_of_5
c)Die Funktionen "^" kann verwendet werden um die Potenz einer Zahl zu berechnen.
d)Nein es führt nicht immer zum gleichen Verhalten wie f, da negative Argumente trotzdem zu einem korrekten Ergebnis führen im Gegensatz zu.
 Ja es ist kritisch, da es bei der Verwendung der Funktion f mit negativen Argumenten zu Problemen führt.
 e)
 i) f :: Int -> Int
    f n 
    | n == 0 = 1
    | n < 0 = (-5) * f (n+1)
    | n > 0 = 5 * f (n-1)

ii) f :: Int -> Int
    f a = 5 ^ a
iii) f :: Int -> Int
     f = \n -> (if n == 0 then 1 else 5 * f (n-1))

f) Ja selbe Bedeutung.
   Man kann klammern einsparen (zur Übersicht?) ?
-}
