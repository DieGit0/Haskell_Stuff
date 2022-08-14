{-# LANGUAGE DataKinds #-}
import Control.Monad
a = [1,2,3]
b = [4,5,6]
c = [9,8,9]

--ain :: IO Integer
main = do
    t <- fmap (read::String->Int) getLine
    forM [1..t] (\_->do
                      a <- getLine -- "1 2 3"
                      --print a
                      let b = filter ( /= ' ') a  -- "123"
                      -- print b     
                      let c = map (:[]) b -- ["1","2","3"]
                      -- print c
                      let d = fmap ( read :: String -> Int) c
                      print d
                      )
                   -- [ a !! i |  i <- [0..length a-1] ]
        --           getArray = array ((1,1),(2,2)) [((1,1),'A'),((1,2),'B'),((2,1),'C'),((2,2),'D')]
    --    n <- fmap (read::String->Int) getLine
    --    forM [1..n] (\_->do fmap ((\[a, b, c]->[a,b,c]).map (read::String->[Int]).words) getLine :: [Int]))

        -- do a <- getLine; print a
        -- b = filter ( /= ' ') a
        -- c = map (:[]) b
        -- d = fmap ( read::String -> Int) c