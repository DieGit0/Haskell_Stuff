{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

t = "12:45:54PM"
-- 12:45:54PM => 19:05:45
-- 12:45:54PM => 12:45:54
-- 12:45:54AM => 00:45:54


timeConversion' :: [Char] -> [Char]
timeConversion' t | xm 'P' = c ++ d
                  | otherwise     = case h of "12" -> "00" ++ d ; _ -> take 8 t
 where h  = take 2 t
       xm = (t !! 8 ==)
       c  = if xm 'P' && h /= "12" then show ((read::String -> Int) h + 12 ) else h
       d  = (.) (take 6) (drop 2) t
