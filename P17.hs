module P17 (speak) where

main = mapM_ (putStrLn . speak) [1..1000]

speak 1000 = "one thousand"
speak x | x `mod` 100 == 0 = word x
speak x | x < 100  = word x
        | x < 1000 = word a ++ " and " ++ word b
            where
                a = x - b
                b = x `mod` 100
                
word 0 = "zero"
word 1 = "one"
word 2 = "two"
word 3 = "three"
word 4 = "four"
word 5 = "five"
word 6 = "six"
word 7 = "seven"
word 8 = "eight"
word 9 = "nine"
word 10 = "ten"
word 11 = "eleven"
word 12 = "twelve"
word 13 = "thirteen"
word 14 = "fourteen"
word 15 = "fifteen"
word 16 = "sixteen"
word 17 = "seventeen"
word 18 = "eighteen"
word 19 = "nineteen"
word 20 = "twenty"
word 30 = "thirty"
word 40 = "forty"
word 50 = "fifty"
word 60 = "sixty"
word 70 = "seventy"
word 80 = "eighty"
word 90 = "ninety"
word x | (20 < x) && (x < 100) = word (x - (x `mod` 10)) ++ "-" ++ word (x `mod` 10)
       | (99 < x) && (x < 1000) = word (x `div` 100) ++ " hundred"