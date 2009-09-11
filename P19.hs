module P19 where

days = iterate nextDay (1901, 1, 1, 2)
    
nextDay (y, m, d, wd) | d < daysInMonth (y, m) = (y, m, d+1, nwd) 
                      | m < 12                 = (y, m+1, 1, nwd)
                      | otherwise              = (y+1, 1, 1, nwd)
    where
        -- 0 = sun, 1 = mon, .. , 6 = sat
        nwd = (succ wd) `mod` 7
               
daysInMonth (y, 2) | isLeapYear y = 29
                   | otherwise    = 28
daysInMonth (_, 4)  = 30
daysInMonth (_, 6)  = 30
daysInMonth (_, 9)  = 30
daysInMonth (_, 11) = 30
daysInMonth (_, _)  = 31

isLeapYear y | (y `mod` 400) == 0 = True
             | (y `mod` 100) == 0 = False
             | otherwise          = (y `mod` 4) == 0
             
             
euler19 = fromIntegral $ length $ filter theDays $ takeWhile is20thCentury days

is20thCentury (y,_,_,_) = y < 2001
theDays (_, _, d, wd) = wd == 0 && d == 1