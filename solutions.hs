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

euler1 = sum [x | x <- [0..999], x `mod` 3 == 0 || x `mod` 5 == 0]

euler2 = sum $ filter even $ takeWhile ((>) 4000000) fibs

euler3 = maximum $ primeFactors 600851475143

euler4 = maximum $ map read $ filter palindrome [show (x * y) | x <- [1..999], y <- [1..999]]

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

euler12 = head $ dropWhile (\x -> (length $ factors x) <= 500) triangleNumbers

euler13 = read $ concat $ map show $ take 10 $ digits $ sum p13

-- use strict foldl otherwise we get a stack overflow
euler14 = snd $ foldl1' max $ map (\n -> ((length $ collatz n), n)) [1..999999]

euler16 = sum $ digits $ 2^1000

euler17 = fromIntegral . length . filter isAlpha . concat $ map speak [1..1000]

euler20 = sum $ digits $ fact 100

euler25 = fromIntegral . length . fst $ break p fibs
    where
        p x = (length $ digits x) == 1000

euler35 = fromIntegral $ length $ filter isCircularPrime $ takeWhile (<1000000) primes

euler48 = (lastDigits 10) . sum $ map pow [1..1000]
    where
        pow x = x^x
        lastDigits n = digitsToInt . (takeLast n) . digits
        digitsToInt = read . concat . (map show)
        takeLast n = reverse . (take n) . reverse


solutions = [ (1, return euler1, 233168),
              (2, return euler2, 4613732),
              (3, return euler3, 6857),
              (4, return euler4, 906609),
              (5, return euler5, 232792560),
              (6, return euler6, 25164150),
              (7, return euler7, 104743),
              (8, return euler8, 40824),
              (9, return euler9, 31875000),
              (10, return euler10, 142913828922),
              (12, return euler12, 76576500),
              (13, return euler13, 5537376230),
              (14, return euler14, 837799),
              (15, return euler15, 137846528820),
              (16, return euler16, 1366),
              (17, return euler17, 21124),
              (18, return euler18, 1074),
              (19, return euler19, 171),
              (20, return euler20, 648),
              (22, euler22, 871198282),
              (25, return euler25, 4782),
              (35, return euler35, 55),
              (48, return euler48, 9110846700),
              (67, return euler67, 7273)
            ]

main = do
    putStrLn $ show (length solutions) ++ " solutions..."
    mapM_ (\x -> format x >>= putStrLn) solutions
        where
            format (n, a, e) = do
                ar <- a
                return $ num ++ (show ar) ++ (check (ar == e))
                where
                    num = (show n) ++ ": "
                    check True = ""
                    check False = "  BZZT!"
