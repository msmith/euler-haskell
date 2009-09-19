module P22 (euler22) where
    
import Util
import Data.Char
import Data.List

euler22 = do
    names <- readWordList "names.txt"
    return $ fromIntegral $ scoreAll names
        where
            strip = init . tail

score :: String -> Int
score cs = sum $ map cScore cs
    where
        cScore c = (ord c) - (ord 'A') + 1

scoreAll :: [String] -> Int
scoreAll xs = sum $ zipWith (*) ys [1..]
    where
        ys = map score $ sort xs