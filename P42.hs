module P42 (euler42) where

import Util
import Data.Char
import Data.List

scoreWords = do
    ws <- readWordList "words.txt"
    return $ map score ws
        where
            strip = init . tail
            score = fromIntegral . sum . map cScore
            cScore c = (ord c) - (ord 'A') + 1

euler42 = do
    ss <- scoreWords
    let maxScore = maximum ss
    let triangleNums = takeWhile (<= maxScore) triangleNumbers
    return $ fromIntegral $ length $ filter (`elem` triangleNums) ss
