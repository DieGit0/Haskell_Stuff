fat::Integer->Integer
fat 0 = 1
fat n = fromIntegral n * fat(n-1)

-- n!/(r! * (n-r)!)

--tp :: Integer -> [Integer]
tp k = t [0..k]
    where t = map (\x -> div (fat x) (fat x * fat(x-x)))

-- [ n | x<-[0..5], n<-[div (fat x) (fat x * fat(x-x))] ]  

--(a -> b -> c) -> [a] -> [b] -> [c]
-- *Main> zipWith (+) [1,2,3] [4,5,6]
--  [5,7,9]

s=[1]:map((1:).(++[1]).(zipWith(+)=<<tail))s

p=[1]:[1:zipWith(+)r t++[1]|r@(_:t)<-p]

pascal = iterate ((++ [1]) . (1:) . (zipWith (+) =<< tail)) [1]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
-- ----------------------------------------------------------------
pascalTriangle = 
    [1] : map nextRow pascalTriangle
  where 
    nextRow = ([1] ++). (++ [1]). pairSum
    pairSum x = zipWith (+) x (tail x)

showPascalTriangle = 
    map listToString pascalTriangle
  where
    listToString = unwords. map show
  
main :: IO ()
main = do 
       getLine >>= putStrLn. unlines. flip take showPascalTriangle. read