lst :: [Int]
lst = [8,15,22,1,10,6,2,18,18,1]
xs :: [Int]
xs = lst
 
-- 1 Approach: Iteration by Lists Comprehension
f :: [Int] -> [Int]
f lst = [ n | ix<-[1,3..(length lst)-1], n <-[(!!) lst ix] ] 

-- 2 Approach: Iteration by Pattern Matching + Recursion
f' :: [Int] -> [Int]
f' (even:odd:lst) = odd : f lst
f' [x] = []
f' [] = []

-- 3 - Aproach
f'' :: [a] -> [a]
f'' lst = map (lst !! ) [1,3..length(lst)-1]

--4 - Aproach
f''' :: [a] -> [a]
f''' lst = [x | (x, y) <- zip lst [0..], odd y]