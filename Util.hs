module Util where

import Data.List
import Data.Char

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

factors :: Integral a => a -> [a]
factors n = nub $ concat $ [[x, (n `div` x)] | x <- [1..end], n `mod` x == 0]
    where
        end = floor . sqrt $ fromIntegral n

prime :: Integral a => a -> Bool
prime x = notElem 0 $ map (x `mod`) [2..m]
    where
        m = floor $ sqrt $ fromIntegral x

primes :: Integral a => [a]
primes = filter prime [2..]

lowestPrimeFactor :: Integral a => a -> a
lowestPrimeFactor = head . filter prime . factors
    where
        prime n = (factors n) == [1, n]

primeFactors :: Integral a => a -> [a]
primeFactors n = primeFactors' [] n
    where
        primeFactors' ps n | prime n = n:ps
                           | otherwise = primeFactors' (lpf:ps) (n `div` lpf)
            where
                lpf = lowestPrimeFactor n

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == (reverse xs)

lcm :: [Integer] -> Integer
lcm = merge . highestOfEachPrime
    where
        merge = product . map mult
        mult (x,e) = x^e
        highestOfEachPrime = map (head . reverse) . groupBy fstEq . sort . concat . atoms
        fstEq x y = (fst x) == (fst y)
        atoms = map freqs . map (sort . primeFactors)
            where
                freqs [] = []
                freqs xs = (x,n) : freqs rest
                    where
                        (same, rest) = break (/= x) xs
                        x = head xs
                        n = length same

lcmCheat :: [Integer] -> Integer
lcmCheat = foldl Prelude.lcm 1

digits :: (Num b, Show a) => a -> [b]
digits n = map (fromIntegral . digitToInt) $ show n

triangleNumbers :: [Integer]
triangleNumbers = tn 1 2
    where
        tn x y = x:(tn (x+y) (y+1))
        
collatz :: (Integral t) => t -> [t]
collatz 1 = [1]
collatz n = n:(collatz next)
    where
        next | even n    = n `div` 2
             | otherwise = 3*n + 1

fact :: (Num t, Enum t) => t -> t
fact n = product [1..n]

uniq :: Eq a => [a] -> [a]
uniq []     = []
uniq [x]    = [x]
uniq (x:xs) | x == head xs = x:(uniq $ tail xs)
            | otherwise = x:(uniq xs)
	
split :: (Eq a) => a -> [a] -> [[a]]
split x = splitWith ((==) x)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith predicate xs =
        pre : case rest of
                [] -> []
                _  -> splitWith predicate $ dropWhile predicate rest
        where
                (pre, rest) = break predicate xs

pascalsTriangle :: [[Integer]]
pascalsTriangle = p' [1]
    where
        p' xs = xs : (p' next)
            where
                next = zipWith (+) padded $ tail padded
                padded = [0] ++ xs ++ [0]