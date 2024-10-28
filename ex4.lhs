> import Numeric (showHex, showIntAtBase)
> import Data.Char (intToDigit, toUpper, digitToInt)


> data IN_1 = Eins | Nf IN_1 deriving Show
> data ZZ   = Null | Plus IN_1 | Minus IN_1 

> type Zett = Integer

converts a number of Zett type to the same number of ZZ type

> von_Zett_nach_ZZ :: Zett -> ZZ
> von_Zett_nach_ZZ z
>   | z == 0 = Null
>   | z <= 0 = Minus (help_Zett_ZZ (-z))
>   | otherwise = Plus (help_Zett_ZZ z)   

helpfunction of von_Zett_nach_ZZ recursively adds a NF until z is 1 then adds Eins

> help_Zett_ZZ :: Zett -> IN_1
> help_Zett_ZZ z 
>   | z > 1 = Nf (help_Zett_ZZ (z-1))
>   | otherwise = Eins

converts a number of ZZ type to the same number of Zett type

> von_ZZ_nach_Zett :: ZZ -> Zett
> von_ZZ_nach_Zett Null = 0
> von_ZZ_nach_Zett (Minus z) = -(help_ZZ_nach_Zett z 1)
> von_ZZ_nach_Zett (Plus z) = help_ZZ_nach_Zett z 1

helpfunction of von_ZZ_nach_Zett recursively increses the required output by 1 until counter equals 1

> help_ZZ_nach_Zett :: IN_1 -> Zett -> Zett
> help_ZZ_nach_Zett Eins n = n
> help_ZZ_nach_Zett (Nf z) n = help_ZZ_nach_Zett z (n+1)


inkrement help function increases the input by 1

> inkrement :: ZZ -> ZZ
> inkrement Null = Plus Eins
> inkrement (Minus Eins) = Null
> inkrement (Plus Eins) = Plus (Nf Eins)
> inkrement (Plus x) = Plus(Nf x)
> inkrement (Minus (Nf x)) = Minus x

dekrement help function decreases the input by 1

> dekrement :: ZZ -> ZZ
> dekrement Null = Minus Eins
> dekrement (Minus Eins) = Minus (Nf Eins)
> dekrement (Plus Eins) = Null
> dekrement (Plus (Nf x)) = Plus x
> dekrement (Minus x) = Minus(Nf x)

plus function to add 2 numbers of the type ZZ

> plus :: ZZ -> ZZ -> ZZ
> plus z1 Null = z1
> plus z1 (Minus x) = plus (dekrement z1) (inkrement (Minus x))
> plus z1 (Plus x) = plus (inkrement z1) (dekrement (Plus x))

minus function decreases the first parameter by the second for numbers of the type ZZ

> minus :: ZZ -> ZZ -> ZZ
> minus z1 Null = z1
> minus z1 (Minus x) = minus (inkrement z1) (inkrement (Minus x))
> minus z1 (Plus x) = minus (dekrement z1) (dekrement (Plus x))

mal function multiplys the first parameter by the second one for numbers of the type ZZ
calls helpfunction for the a non basic case of multiplication

> mal :: ZZ -> ZZ -> ZZ
> mal Null z2 = Null
> mal z1 Null = Null
> mal (Plus x) (Plus Eins) = (Plus x)
> mal (Plus x) (Minus Eins) = (Minus x)
> mal (Minus x) (Plus Eins) = (Minus x)
> mal (Minus x) (Minus Eins) = (Plus x)
> mal z1 z2 = help_mal1 z1 z2 z1

help function of mal recursively adds the third parameter to the first until the second equals the value of |1|

> help_mal1 :: ZZ -> ZZ -> ZZ -> ZZ
> help_mal1 z1 (Minus Eins) z3 = z1
> help_mal1 z1 (Plus Eins) z3 = z1
> help_mal1 (Plus x) (Plus y) z3 = help_mal1 (plus (Plus x) (z3)) (dekrement (Plus y)) z3
> help_mal1 (Plus x) (Minus y) z3 = convert_to_negative((help_mal1 ((plus (Plus x) (z3))) (inkrement (Minus y)) z3)) 
> help_mal1 (Minus x) (Minus y) z3 = convert_to_positive((help_mal1 ((plus (Minus x) (z3))) (inkrement (Minus y)) z3))
> help_mal1 (Minus x) (Plus y) z3 = convert_to_negative((help_mal1 ((plus (Minus x) (z3))) (dekrement (Plus y)) z3))

help function to convert a positive number of type ZZ to a negative, Null or negatives dont change

> convert_to_negative :: ZZ -> ZZ
> convert_to_negative (Plus x) = (Minus x)
> convert_to_negative (Minus x) = (Minus x)
> convert_to_negative Null = Null

help function to convert a negative number of type ZZ to a positiven Null or positives dont change

> convert_to_positive :: ZZ -> ZZ
> convert_to_positive (Plus x) = (Plus x)
> convert_to_positive (Minus x) = (Plus x)
> convert_to_positive Null = Null

durch function divides the first parameter by the second one following the rounding rules of the exercise sheet for numbers of the type ZZ
calls helpfunction if for the non basic divisions

> durch :: ZZ -> ZZ -> ZZ
> durch z1 z2
>   | z2 `gleich` Null || z1 `gleich` Null = Null
>   | (convert_to_positive z2) `gleich` (convert_to_positive z1) = if ((z1 `kleiner` Null && z2 `kleiner` Null) || (z1 `groesser` Null && z2 `groesser` Null)) then (Plus Eins) else (Minus Eins)
>   | (convert_to_positive z2) `gleich` (Plus Eins) = if ((z1 `kleiner` Null && z2 `kleiner` Null) || (z1 `groesser` Null && z2 `groesser` Null)) then (convert_to_positive z1) else (convert_to_negative z1)
>   | z1 `kleiner` Null = help_durch z1 z2 (True) (Null)
>   | otherwise = help_durch z1 z2 (True) (Minus Eins)

help function of durch recursively reduces the first parameter by second one until the the value of the first parameter would be negative (third paramter checks that) z4 is the counter

> help_durch :: ZZ -> ZZ -> Bool -> ZZ -> ZZ
> help_durch _ _ False z4 = z4
> help_durch (Plus x) (Plus y) z3 z4 = help_durch ((Plus x) `minus` (Plus y)) (Plus y) (((Plus x) `minus` (Plus y)) `groesser` Null) (inkrement z4)
> help_durch (Minus x) (Minus y) z3 z4 = help_durch ((Minus x) `minus` (Minus y)) (Minus y) (((Minus x) `minus` (Minus y)) `kleiner` Null) (inkrement z4)
> help_durch (Plus x) (Minus y) z3 z4 = convert_to_negative (help_durch ((Plus x) `plus` (Minus y)) (Minus y) (((Plus x) `plus` (Minus y)) `groesser` Null) (inkrement z4))
> help_durch (Minus x) (Plus y) z3 z4 = convert_to_negative (help_durch ((Minus x) `plus` (Plus y)) (Plus y) (((Minus x) `plus` (Plus y)) `kleiner` Null) (inkrement z4))


checks if two numbers of the ZZ are the same

> gleich   :: ZZ -> ZZ -> Bool
> gleich z1 z2 = if (show(z1) == show(z2)) then True else False

checks if two numbers of the ZZ are not the same

> ungleich :: ZZ -> ZZ -> Bool
> ungleich z1 z2 = if (show(z1)==show(z2)) then False else True

checks if paramter one (number of type ZZ) is bigger than the second one

> groesser :: ZZ -> ZZ -> Bool
> groesser Null Null = False
> groesser Null (Minus x) = True
> groesser (Minus x) Null = False
> groesser Null (Plus x) = False
> groesser (Plus x) Null = True
> groesser (Plus x) (Minus y) = True
> groesser (Minus x) (Plus y) = False
> groesser (Plus x) (Plus y) = if fromEnum (Plus x) > fromEnum (Plus y) then True else False
> groesser (Minus x) (Minus y) = if fromEnum (Minus x) > fromEnum (Minus y) then True else False

checks if paramter one (number of type ZZ) is smaller than the second one

> kleiner  :: ZZ -> ZZ -> Bool
> kleiner Null Null = False
> kleiner Null (Minus x) = False
> kleiner (Minus x) Null = True
> kleiner Null (Plus x) = True
> kleiner (Plus x) Null = False 
> kleiner (Plus x) (Minus y) = False
> kleiner (Minus x) (Plus y) = True
> kleiner (Plus x) (Plus y) = if fromEnum (Plus x) < fromEnum (Plus y) then True else False
> kleiner (Minus x) (Minus y) = if fromEnum (Minus x) < fromEnum (Minus y) then True else False

checks if paramter one (number of type ZZ) is bigger than the second one or if they are the same

> ggleich  :: ZZ -> ZZ -> Bool
> ggleich z1 z2 = if gleich z1 z2 || groesser z1 z2 then True else False

checks if paramter one (number of type ZZ) is smaller than the second one or if they are the same

> kgleich  :: ZZ -> ZZ -> Bool
> kgleich z1 z2 = if gleich z1 z2 || kleiner z1 z2 then True else False


Aufgabe 4

> instance Eq (ZZ) where
>   (==) x y = gleich x y
>   (/=) x y = ungleich x y

> instance Ord (ZZ) where
>   (>=) x y = ggleich x y
>   (>) x y = groesser x y
>   (<=) x y = kgleich x y
>   (<) x y = kleiner x y

> instance Enum (ZZ) where
>   fromEnum x = fromIntegral (von_ZZ_nach_Zett x)
>   toEnum x = von_Zett_nach_ZZ (toInteger x)

> instance Num (ZZ) where
>   (+) x y = plus x y
>   (-) x y = minus x y
>   (*) x y = mal x y
>   abs x = convert_to_positive x
>   signum x = if x == Null then Null else if x <= (Minus Eins) then (Minus Eins) else (Plus Eins)
>   fromInteger x = von_Zett_nach_ZZ x

> instance Show (ZZ) where
>   show x = if fromEnum x < 0 then ("-" ++ (map toUpper . (showHex (-fromEnum x))) "" ) else (map toUpper . (showHex (fromEnum x))) ""
    
> class Binaer a where
>   a_zu_binaer :: a -> String
>   binaer_zu_a :: String -> a

> instance Binaer (ZZ) where
>   a_zu_binaer x = if fromEnum x < 0 then "-" ++ showIntAtBase 2 intToDigit(-fromEnum x) "" else  showIntAtBase 2 intToDigit(fromEnum x) ""
>   binaer_zu_a (x : xs) = if x == '-' then toEnum(-(help_filter (xs)))::ZZ else toEnum(help_filter ( ([x] ++ xs)))::ZZ

> to_decimal :: [Int] -> Int
> to_decimal [] = 0
> to_decimal (x:xs) = (x * 2 ^(length xs)) + (to_decimal xs)

> help_filter :: [Char] -> Int
> help_filter x = if (length (filter (== '0') x) + length (filter (== '1') x)) < length x then 0 else to_decimal (map digitToInt(x))

> instance Binaer (IN_1) where
>   a_zu_binaer x = (showIntAtBase 2 intToDigit(help_ZZ_nach_Zett  x 1) "")
>   binaer_zu_a (x : xs) = if x == '-' then Eins else help_Zett_ZZ1 (help_filter( ([x] ++ xs)))

> help_Zett_ZZ1 :: Int -> IN_1
> help_Zett_ZZ1 z 
>   | z > 1 = Nf (help_Zett_ZZ1 (z-1))
>   | otherwise = Eins

> instance Binaer (Integer) where
>   a_zu_binaer x = if (fromIntegral x) < 0 then "-" ++ (showIntAtBase 2 intToDigit(-fromIntegral x) "") else showIntAtBase 2 intToDigit(fromIntegral x) ""
>   binaer_zu_a (x : xs) = if x == '-' then toInteger(-help_filter((xs))) else toInteger(help_filter(([x] ++ xs)))