
ar :: [Integer]
ar = [1000000001, 1000000002, 1000000003, 1000000004, 1000000005]


aVeryBigSum :: (Foldable t, Num a) => t a -> a
aVeryBigSum ar = sum ar

aVeryBigSum' :: Num p => [p] -> p
aVeryBigSum' [] = 0
aVeryBigSum' (n:ar) = ((n + ) . aVeryBigSum') ar
   

main :: IO ()
main = undefined -- print $ aVeryBigSum'' ar