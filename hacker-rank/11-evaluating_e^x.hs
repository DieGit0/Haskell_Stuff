fat::Integer -> Double
fat 0 =  1
fat n = fromInteger n * fat(n-1)

f ::Double -> Double
f x = 1 + x +
      x^^2 / fat 2 +
      x^^3 / fat 3 +
      x^^4 / fat 4 +
      x^^5 / fat 5 +
      x^^6 / fat 6 +
      x^^7 / fat 7 +
      x^^8 / fat 8 +
      x^^9 / fat 9

f' ::Double -> Double
f' x = 1 + x +
      x^^2 / fat 2 +
      x^^3 / fat 3 +
      x^^4 / fat 4 +
      x^^5 / fat 5 +
      x^^6 / fat 6 +
      x^^7 / fat 7 +
      x^^8 / fat 8 +
      x^^9 / fat 9
 where fat' n | n == 0 = 1
              | otherwise = n * fat'(n-1)

f'' :: Double -> Double
f'' x =  (1 + x) + sume
 where sume = sum $ map (\n -> x^^n / fat n) [2..9]

 --              (.) :: (b -> c) -> (a -> b) -> a -> c
f3 :: Double -> Double
f3 x = foldr ((+) . (\n -> x^^n / fat n)) (1+x) [2..9]

f4 :: Double -> Double
f4 x = foldr ((+) . (\n ->  x^^n / fat n)) (1+x) [2..9]
 where fat n | n == 0 = 1
             | otherwise = fromInteger n * fat(n-1)


 