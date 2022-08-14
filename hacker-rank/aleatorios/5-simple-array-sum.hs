simpleArraySum :: [Int] -> Int
simpleArraySum [] = 0
simpleArraySum (n:ar) = ((n + ) . sum') ar
   where sum' = simpleArraySum