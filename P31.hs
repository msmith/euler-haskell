module P31 (euler31) where

euler31 = fromIntegral $ length $ change 200 coins

coins = [200,100,50,20,10,5,2,1]

change r [1]    = [[(1,r)]]
change r (c:cs) = concatMap nexts ns
    where
        nexts n = map ((c,n):) $ change (r-c*n) cs
        ns = [0..r `div` c]
