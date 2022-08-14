{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

xs :: [(Int, Int)]
xs = [(1,1),(2,2),(3,3)]

valid :: [(Int, Int)] -> Bool
valid (t:xy) = (x `div` y) * y + (x `mod` y) == x
       where x = fst t
             y = snd t