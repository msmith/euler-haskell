module P24 where

import Util

lexPermutation [] n = []
lexPermutation xs n = d:lexPermutation ys n'
    where
        n' = n - d * fact len
        ys = filter (/=d) xs
        d = xs!!((n `div` factorial) `mod` len)
        len = length xs
        factorial = fact $ len - 1

