
repx n a = do 
            r <- [ a | _<-[1..n]]
            r
spacex n = do 
            s <- [ " " | _<-[1..n]]
            s

f n = do
      i  <- [1..n]
      sr <- spacex (n-i) ++ (repx i "#") ++ "\n"
      return sr
            

                        
main :: IO ()
main = do
       putStrLn $ f 6