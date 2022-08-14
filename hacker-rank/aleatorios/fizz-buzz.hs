
xs :: [Integer]
xs = [1..100]

f ::Integral n => [n] -> String
f [] = []
f (x:xs) | mod15 = "FizzBuzz " ++ f xs -- mod3 && mod5 = 
         | mod3  = "Fizz"      ++ f xs
         | mod5  = "Buzz"      ++ f xs
         | otherwise = num     ++ f xs
    where
      mod3 = mod x 3 == 0
      mod5 = mod x 5 == 0
      mod15 = mod x 15 == 0
      num =  show (fromIntegral x) ++ " "
--      fizz xs = [n | n<-xs, mod n 3 == 0]
--      buzz xs = [n | n<-xs, mod n 5 == 0]
--      fb   xs = [n | n<-xs, mod n 15 == 0]