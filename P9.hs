module P9 where

-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a^(2) + b^(2) = c^(2)
-- 
-- For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).
pythagoreanTriplet :: (Num a, Ord a) => (a, a, a) -> Bool
pythagoreanTriplet (a,b,c) = (a < b) && (b < c) && (a^2 + b^2 == c^2)

-- Umm..
tripletsWithSum n = [(a,b,c) | a <- [1..(n-2)], b <- [(a+1)..(n-a-1)], c <- [(b+1)..(n-b-a)], a + b + c == n]

pythagoreanTripletsWithSum = filter pythagoreanTriplet . tripletsWithSum
