module P39 (euler39) where
    
import Data.List (nub, maximumBy)
import Data.Ord (comparing)

combinations p = [t |
    a <- [1..p `div` 3],
    b <- [a..p-a],
    c <- [p-a-b],
    let t = (a,b,c),
    a + b + c == p,
    rightTriangle t]

rightTriangle (a,b,c) = a*a + b*b == c*c

solve p = maximumBy (comparing snd) $ map (\p -> (p, length $ combinations p)) [1..p]

euler39 = fst $ solve 1000