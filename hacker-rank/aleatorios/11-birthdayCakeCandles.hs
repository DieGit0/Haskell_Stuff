
arr = [3, 2, 1, 3]

birthdayCakeCandles :: Ord a => [a] -> Int
birthdayCakeCandles candles = length $ filter ( == maximum candles) candles

