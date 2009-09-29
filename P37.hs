module P37 (euler37) where

import Util
import Data.List

truncatablePrime = all prime . truncations

truncations x = map undigits $ nub $ filter (not . null) $ inits d ++ tails d
    where
        d = digits x

euler37 = sum $ take 11 $ filter truncatablePrime [9..]
