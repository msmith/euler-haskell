module P28 (euler28) where
    
euler28 = solve 1001

solve n = sum $ takeWhile (<= n^2) series

series = 1 : f 1 skips
    where
        skips = concatMap (replicate 4) $ iterate (+2) 2
        f x ss = n:(f n ns)
            where
                (s:ns) = ss
                n = x + s
