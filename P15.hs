module P15 where

import Util

-- For an NxN grid, it takes 2n steps to get from top-left to bottom-right.
--   There must be n Rightward steps and n Downward steps.
-- Therefore, the number of paths = C(2n,n)

binomialCoefficient n k | k < 0 || k > n = 0
                        | otherwise      = fact n `div` (fact k * fact (n-k))

euler15 = numPaths 20
    where
        numPaths n = binomialCoefficient (2*n) n