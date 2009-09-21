module P45 where

euler45 = head $ dropWhile (<=40755) [x | x <- hexs, (find tris x) && (find pents x)]
    where
        find ns n = n == (head $ dropWhile (< n) ns)
        tris  = map tri [1..]
        pents = map pent [1..]
        hexs  = map hex [1..]
        tri n  = n*(n+1) `div` 2
        pent n = n*(3*n-1) `div` 2
        hex n  = n*(2*n-1)