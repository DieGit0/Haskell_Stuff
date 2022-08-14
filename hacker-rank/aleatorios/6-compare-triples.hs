{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
a = [17, 28, 30]
b = [99, 16, 8]

compareTriplets :: (Ord a, Num a) => [a] -> [a] -> [a]
compareTriplets a b = [ali] ++ [bob]
    where ali = sum [ 1 | (a,b) <- zip a b, a > b]
          bob = sum [ 1 | (a,b) <- zip a b, b > a]



main :: IO ()
main = print $ compareTriplets a b