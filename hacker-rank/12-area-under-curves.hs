solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = sum $ map (\t -> fst t *( fst t ^ snd t)) $ zip a b