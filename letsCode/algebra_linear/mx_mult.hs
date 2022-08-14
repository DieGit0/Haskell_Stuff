import Data.Matrix

m1 = [[1,2],
      [4,5]]

m2 = [[1,2],
      [4,5]]

{--mx_mul :: (Num ix, Enum ix) => [[a]] -> [(a, ix)]
--mx_mul (mx) = [(x,y)]--[ (fst a) *  m !! (snd a) * t ]
            where x = fst $ head xy
                  y = snd $ head xy
                  xy = [(n,ix) | (n,ix) <- zip (mx_elems mx) [0..]]
            --      b = m !! (ix * t)
                  m = mx_elems mx
                  t = div (length (mx_elems mx)) 2
--}
---mx_mul :: (Num ix, Enum ix) => [[a]] -> t --[(a, ix)]
mx_mul :: [[a]] -> [a]
mx_mul []   = []
mx_mul (mx) = do
               let t = div (length (mx_elems mx)) 2
               [t]
--               where x = fst $ head xy
--                  y = snd $ head xy
--                  xy = [(n,ix) | (n,ix) <- zip (mx_elems xm) [0..]]
            --      b = m !! (ix * t)
--                  m = mx_elems mx
--                  t = div (length (mx_elems mx)) 2

-- Percorre a matriz de matrizES e retona seus elementos
-- Param: matriz do tipo lista
-- BigO(n * n(n))
mx_elems mx = [n | m <- mx, n <-m]

main = do print $ mx_mul m1
