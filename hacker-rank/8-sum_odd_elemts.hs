a :: [Int]
a = [1,2..10]

f :: [Int] -> Int
f arr = sum $ [x | x<-arr, odd x]

-- No use of mod function
f' :: [Int] -> Int
f' arr = sum $ [x | x<-arr, (div x 2) * 2 /= x]

--Recursion
f'' :: [Int] -> Int
f'' [] = 0
f'' (n:arr) | (div n 2) * 2 /= n = (+) n (f'' arr)
            | otherwise = (f'' arr)

-- This part handles the Input/Output and can be used as it is. Do not change or modify it.
main :: IO ()
main = do
        inputdata <- getContents
        putStrLn $ show $ f $ map (read :: String -> Int) $ lines inputdata