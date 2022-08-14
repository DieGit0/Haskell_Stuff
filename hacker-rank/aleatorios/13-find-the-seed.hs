-- Complete the 'findSeed' function below.
--
-- The function is expected to return an INTEGER_ARRAY.
-- The function accepts following parameters:
--  1. INTEGER k
--  2. INTEGER_ARRAY f
--  3. INTEGER_ARRAY c
--

findSeed k f c = fk
        where fk = [ (k,n) | (k,n)<- zip f c ]
 --             cn = [ n | n <- c ] 