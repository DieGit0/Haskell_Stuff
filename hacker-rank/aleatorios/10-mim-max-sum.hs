
arr = [7, 69, 2, 221, 8974]
-- 299 9271

miniMaxSum :: (Show a, Num a) => [a] -> IO ()
miniMaxSum arr =  putStrLn $ show (sum $ init arr) ++ " " ++  show (sum $ tail arr)
             where mi = undefined
                   mx = undefined

miniMaxSum' :: (Show a, Num a, Ord a) => [a] -> IO ()
miniMaxSum' arr =  putStrLn $ show (sum arr - mx) ++ " " ++  show (sum arr - mi)
            where mi = minimum arr
                  mx = maximum arr 

main :: IO ()
main = miniMaxSum' arr