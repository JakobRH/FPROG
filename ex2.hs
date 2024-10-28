type Nat0 = Int
type Nat1 = Int

reverse1 :: Int -> Int
reverse1 = read . reverse . show

--1A
dp :: (Nat1, Nat1) -> [Nat1]
dp (x,y) = if x < y then [n | n <- [x..y], is_prime n && is_prime (reverse1 n)] else [n | n <- [y..x], is_prime n && is_prime (reverse1 n)]

is_prime :: Nat1 -> Bool
is_prime x 
    | x == 1 = False
    | x == 2 = True
    | otherwise = if (length [n | n <- [2..(x-1)], mod x n == 0]) > 0 then False else True


--2A
folge :: Nat1 -> [Nat1]
folge n = help_folge n []

help_folge :: Nat1 -> [Nat1] ->[Nat1]
help_folge n xs
    | xs == [] = xs ++ [n] ++ help_folge n [n]
    | (last xs) == 1 = []
    | otherwise = if not (is_element (real_divider (last xs)) xs) then [real_divider(last xs)] ++ help_folge n (xs ++ [real_divider(last xs)]) else []


is_element :: Int -> [Int] -> Bool
is_element n [] = False
is_element n (x : xs)
    | n == x = True
    | otherwise = is_element n xs

real_divider :: Nat1 -> Nat1
real_divider n = (sum [x | x <- [1..(n-1)], mod n x == 0])

--3A
medianoid :: [Int] -> Int
medianoid xs
    | xs == [] = 0 
    | not (in_pairs_different xs) = sum xs
    | otherwise = xs !! (find_medianoid xs 0)

find_medianoid :: [Int]-> Int -> Int
find_medianoid xs x = if ((length [n | n <- xs, n < xs !! x] - length[m | m <- xs, m > xs !! x]) <= 1 && (length [n | n <- xs, n < xs !! x] - length[m | m <- xs, m > xs !! x]) >= 0) then x else find_medianoid xs (x+1)


in_pairs_different :: [Int] -> Bool
in_pairs_different xs
    | length xs == 1 = True
    | otherwise = if not (is_element (head xs) (tail xs)) then in_pairs_different (tail xs) else False