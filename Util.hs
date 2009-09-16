module Util where

import Data.List
import Data.Char

fibs :: (Num a) => [a]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

factors :: (Integral a) => a -> [a]
factors n = nub $ concat $ [[x, (n `div` x)] | x <- [1..end], n `mod` x == 0]
    where
        end = floor . sqrt $ fromIntegral n

prime :: (Integral a) => a -> Bool
prime n = all notDivisible [2..m]
    where
        notDivisible x = n `mod` x /= 0
        m = floor $ sqrt $ fromIntegral n

primes :: (Integral a) => [a]
primes = 2 : filter prime [3,5..]

lowestPrimeFactor :: (Integral a) => a -> a
lowestPrimeFactor = head . filter prime . factors
    where
        prime n = (factors n) == [1, n]

primeFactors :: (Integral a) => a -> [a]
primeFactors n = primeFactors' [] n
    where
        primeFactors' ps n | prime n = n:ps
                           | otherwise = primeFactors' (lpf:ps) (n `div` lpf)
            where
                lpf = lowestPrimeFactor n

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

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
lcmCheat = foldl1 Prelude.lcm

digits :: (Integral t) => t -> [t]
digits n = f n []
    where
        f 0 xs = xs
        f y xs = f d (m:xs)
            where
                (d, m) = y `divMod` 10

undigits :: [Integer] -> Integer
undigits = foldl (\x y -> x*10 + y) 0

triangleNumbers :: [Integer]
triangleNumbers = scanl1 (+) [1..]
        
collatz :: (Integral t) => t -> [t]
collatz 1 = [1]
collatz n = n : collatz next
    where
        next | even n    = n `div` 2
             | otherwise = 3*n + 1

fact :: (Num t, Enum t) => t -> t
fact n = product [1..n]
	
split :: (Eq a) => a -> [a] -> [[a]]
split x = splitWith (== x)

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

isCircularPrime :: Integer -> Bool         
isCircularPrime x = all prime $ map undigits $ rotations $ digits x

rotations :: [a] -> [[a]]
rotations xs = map (rotateAt xs) $ [1..length xs]
    where
        rotateAt xs i = flipIt $ splitAt i xs
        flipIt (a, b) = b ++ a

