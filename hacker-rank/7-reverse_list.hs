l = [1..10]
--rev :: [a] -> [a]
--rev l = reverse l

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

rev' :: [a] -> [a]
rev' l = [ l !! (i-1) | i <- [length l, length l - 1 .. 1]]