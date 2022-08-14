--{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--main =
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Bits (xor)
xs :: [Int]
xs = [1,2,3,4,3,2,1]
xy :: [Int]
xy = [1,1,2]
xz :: [Int]
xz = [0,0,1,2,1]
-- Diego de Franco Matos (Brazil)
-- abordagem: Comutação numérica
lonelyinteger::[Int] -> Int
lonelyinteger [x] = x  -- [ 0 0 1 2 1] =>[1 2 1] =>[2 1 1] =>[1 1 2] -> 2
lonelyinteger (x:y:xs) | x==y = lonelyinteger xs
                       | otherwise =  lonelyinteger (y:xs++[x])

--Xor
lonelyinteger2 a = foldr xor 0 a
