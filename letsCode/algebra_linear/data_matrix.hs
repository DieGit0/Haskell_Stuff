import Data.Matrix

-- https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html#v:matrix

m1 = matrix 4 4 $ \(i,j) -> 2*i - j

m2 = fromList 3 3 [1..]

m3 = toList m2

m4 = transpose m2

m5 = multStd2 m2 m2

--diagonalList

main = do
        print m1
        print m2
        print m3
        print m4
        print m5
