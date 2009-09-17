module P30 (euler30) where

import Util

euler30 = fromIntegral $ sum $ filter p [2..n*9^n]
    where
        p x = x == (sum $ zipWith (!!) (repeat fifths) (digits x))
        fifths = map (^n) [0..9]
        n = 5