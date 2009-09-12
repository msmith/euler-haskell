module P52 (euler52) where

import Data.List
import Data.Maybe
import Util

euler52 = solve 6

solve n = fromJust $ find p [1..]
    where
        p x = (length $ nub $ map (sort . digits) $ xs) == 1
            where
                xs = zipWith (*) (repeat x) [1..n]