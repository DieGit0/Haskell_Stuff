import Data.Bits (xor)

headSafe [] = []
headSafe (x:xs) = x

xs = ['a','b','c','d','b','c','a','a'] -- se tiver mais de 1 ocorrencia nao pega , so pega a primeira
f :: Eq a => [a] -> [a]
f [] = []
f [x] = [x]
f (x:xs) = x : f ( (take (head $ ixList) xs) ++ drop ((head $ ixList)+1) xs )
          where ixList | [i | (c,i) <- zip xs [0..], x == c] == []  = [-1]  -- retona [-1] caso nao encontre ocorrencia
                       | otherwise = [i | (c,i) <- zip xs [0..], x == c]    -- repetido


f2 (x:y:xs) | x /= y = x : f2 (y:xs)
            | otherwise = x : f2 (xs++[y])

f3 :: Eq a => [a] -> [a]
f3 [] = []
f3 (s:xs) | not (elem s xs)  = s: f3 xs
          | otherwise    = f3 xs++[s]

--f3 xs = foldr xor 'a' xs
