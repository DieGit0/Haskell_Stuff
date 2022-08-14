l = [1,1,1,1,1]
len :: [a] -> Int
len [] = 0
len (_:lst) = (+) 1 (len lst)

len' :: [a] -> Int
len' lst = sum [ 1 | _ <- lst ]

len'' :: Num n => [n]  ->  n
len'' = foldl (+) 0 . map (const 1)

len3 :: Num n => [n]  ->  n
len3 = sum . map (const 1)

len4 :: [a] -> Int
len4 = foldr (\ _ -> (+) 1) 0 

len5 :: [b] -> Integer
len5 = head . reverse . zipWith (\x y -> x) [1..]

len6 :: [b] -> Integer
len6 lst =  snd $ (!!) (reverse $ zip lst [1..]) 0

len7 :: (Num a, Enum a) => [b] -> a
len7 lst =  fst(last(zip [1..] lst))

len8 :: [b] -> Integer
len8 =  fst . last . zip [1..] 