
-- ** Operações com Matrize **
--Matriz com Listas
mx1 = [[1,2,3],
       [4,5,6],
       [7,8,9]]

mx2 = [[1,2,3],
       [4,5,6],
       [7,8,9]]

-- Percorre a matriz de matrizES e retona seus elementos
-- Param: matriz do tipo lista
-- BigO(n * n(n))
mx_elems mx = [n | m <- mx, n <-m]

-- Recebe Linha e Coluna e retorna o elemento em l c
mx_get_ele mx l c = (!!) (mx !! l) c

-- Recebe Linha e Coluna e retorna os pares de elemento em l c correnpondente entre as duas colunas
mx_get_par l c m1 m2 =  ((!!) (m1 !! l) c, (!!) (m2 !! l) c)

-- zipa o par de elemento entre duas matrizes
-- BigO(n)
mx_zip m1 m2 = [ x | x <- zip (mx_elems m1) (mx_elems m2) ]

-- Aplica operações aritméticas em matrizes
-- Ex: mx_zip (+) mx1 mx2 => [2,4,6,8,10,12,14,16,18]
-- Ex: mx_zip (-) mx1 mx2 => [0,0,0,0,0,0,0,0,0]
-- Ex: mx_zip (div) mx1 mx2 => [1,1,1,1,1,1,1,1,1]
--
mx_op f m1 m2 = [ f i j | (i,j) <- mx_zip m1 m2]

-- Matriz por um escalar (aplica operação a cada elemento de mx por um dado n)
mx_esc f mx n = [ f i n | i <- mx_elems mx]

main = do
        putStrLn "Matriz com Listas"
        print mx1
        print mx2
        print $ mx_elems mx1
        print $ mx1 !! 0
        print $ mx2 !! 2
        print $ mx_get_ele mx1 0 0
        print $ mx_get_ele mx2 2 2
        print $ mx_get_par 0 0 mx1 mx2
        print $ mx_get_par 1 1 mx1 mx2
        print $ mx_zip mx1 mx2
        putStrLn "Operações:"
        print $ mx_op (+) mx1 mx2
        print $ mx_op (-) mx1 mx2
        print $ mx_op (*) mx1 mx2
        print $ mx_op (div) mx1 mx2
        putStrLn "Escalar:"
        print $ mx_esc (*) mx1 2
        print $ mx_esc (*) mx1 (-1)
        print $ mx_esc (div) mx1 2
