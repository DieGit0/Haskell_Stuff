{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad ( forM )

valid :: [(Int, Int)] -> Bool
valid [] = False
valid (t:xy) = (x `div` y) * y + (x `mod` y) == x -- erro no final dos testes divisao por zero
       where x = fst t
             y = snd t

main = do
    t <- fmap (read::String->Int) getLine
    forM [1..t] (\_->do
        n <- fmap (read::String->Int) getLine
        func <- forM [1..n] (\_->do fmap ((\[a, b]->(a,b)).map (read::String->Int).words) getLine :: IO (Int, Int))
        putStrLn $ if valid func then "YES" else "NO")


--f :: (Integer x y) => (x,y) -> [Char]
{--
f :: (Eq a, Num a, Integral a) => (a, a) -> [Char]
f (x,y)  | x == y = "YES"
         | x * 2 == y = "YES"
         | (x `div` y) *y + (x `mod` y) == x = "YES"
         | otherwise = "NO" 
    
        
main = do
        -- (x,y) <-  readLn::IO (Int, Int)
        getLine >>= putStrLn . f . read
       --  (.) print f (x,y)
--}