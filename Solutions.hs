module Solutions where
    
import Util

import Data.Char
import Data.List
import Data.Maybe

import P8
import P9
import P13
import P15
import P17
import P18
import P19
import P22
import P24
import P28
import P30
import P31
import P37
import P39
import P42
import P45
import P52

euler1 = sum [x | x <- [0..999], x `mod` 3 == 0 || x `mod` 5 == 0]

euler2 = sum $ filter even $ takeWhile (< 4000000) fibs

euler3 = maximum $ primeFactors 600851475143

euler4 = maximum $ map undigits $ filter palindrome [digits (x * y) | x <- ns, y <- ns]
    where
        ns = [100..999]

euler5 = Util.lcm [2..20]

euler6 = (square $ sum nums) - (sum $ map square nums)
    where
        square x = x^2
        nums = [1..100]

euler7 = primes!!(10001-1)

euler8 = maximum $ map product $ subseqs 5 p8

euler9 = a * b * c
    where
        (a, b, c) = head $ pythagoreanTripletsWithSum 1000

euler10 = sum $ takeWhile (< 2000000) primes

euler12 = head $ dropWhile ((<= 500) . length . factors) triangleNumbers

euler13 = undigits $ take 10 $ digits $ sum p13

-- use strict foldl otherwise we get a stack overflow
euler14 = snd $ foldl1' max $ map (\n -> ((length $ collatz n), n)) [1..999999]

euler16 = sum $ digits $ 2^1000

euler17 = fromIntegral . length . filter isAlpha . concat $ map speak [1..1000]

euler20 = sum $ digits $ fact 100

euler24 = undigits $ map fromIntegral $ lexPermutation [0..9] (1000000-1)

euler25 = fromIntegral . (1+) . length $ takeWhile p fibs
    where
        p x = (length $ digits x) < 1000

euler35 = fromIntegral $ length $ filter isCircularPrime $ takeWhile (<1000000) primes

euler36 = sum $ [x | x <- [0..999999], palindrome (digits x) && palindrome (digitsBase 2 x)]

euler40 = product $ map (\x -> d!!(x-1)) [1,10,100,1000,10000,100000,1000000]
    where
        d = concat $ map digits [1..1000000]

euler48 = (lastDigits 10) . sum $ map pow [1..1000]
    where
        pow x = x^x
        lastDigits n = undigits . (takeLast n) . digits
        takeLast n = reverse . (take n) . reverse


euler53 = fromIntegral $ length $ filter (>1000000) xs
    where
        xs = [binomialCoefficient n r | n <- [1..100], r <- [1..n]]
        
euler56 = maximum $ map (sum . digits) [a^b | a <- ns, b <- ns]
    where
        ns = [1..100]

solutions = [ Solution 1 (return euler1) 233168,
              Solution 2 (return euler2) 4613732,
              Solution 3 (return euler3) 6857,
              Solution 4 (return euler4) 906609,
              Solution 5 (return euler5) 232792560,
              Solution 6 (return euler6) 25164150,
              Solution 7 (return euler7) 104743,
              Solution 8 (return euler8) 40824,
              Solution 9 (return euler9) 31875000,
              Solution 10 (return euler10) 142913828922,
              Solution 12 (return euler12) 76576500,
              Solution 13 (return euler13) 5537376230,
              Solution 14 (return euler14) 837799,
              Solution 15 (return euler15) 137846528820,
              Solution 16 (return euler16) 1366,
              Solution 17 (return euler17) 21124,
              Solution 18 (return euler18) 1074,
              Solution 19 (return euler19) 171,
              Solution 20 (return euler20) 648,
              Solution 22 euler22 871198282,
              Solution 24 (return euler24) 2783915460,
              Solution 25 (return euler25) 4782,
              Solution 28 (return euler28) 669171001,
              Solution 30 (return euler30) 443839,
              Solution 31 (return euler31) 73682,
              Solution 35 (return euler35) 55,
              Solution 37 (return euler37) 748317,
              Solution 36 (return euler36) 872187,
              Solution 39 (return euler39) 840,
              Solution 40 (return euler40) 210,
              Solution 42 euler42 162,
              Solution 45 (return euler45) 1533776805,
              Solution 48 (return euler48) 9110846700,
              Solution 52 (return euler52) 142857,
              Solution 56 (return euler56) 972,
              Solution 53 (return euler53) 4075,
              Solution 67 (return euler67) 7273
            ]

data Solution = Solution
    { num      :: Integer
    , solution :: IO Integer
    , expected :: Integer
    }