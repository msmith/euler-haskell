module P45 (euler45) where

euler45 = (series tris pents hexs)!!2
    where
        tris  = map tri [1..]
        pents = map pent [1..]
        hexs  = map hex [1..]

tri n  = n*(n+1) `div` 2
pent n = n*(3*n-1) `div` 2
hex n  = n*(2*n-1)

series (a:as) (b:bs) (c:cs)
    | a == b && a == c = a:(series as bs cs)
    | a < b || a < c   = series as (b:bs) (c:cs)
    | b < a || b < c   = series (a:as) bs (c:cs)
    | c < a || c < b   = series (a:as) (b:bs) cs