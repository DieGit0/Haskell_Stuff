a :: [Int]
a = [1,2,-3,-4,5] 

f :: (Ord a, Num a) => [a] -> [a]
f [] = []
f (x:xs) | x < 0 = x * (-1) : f xs
         | otherwise = x : f xs