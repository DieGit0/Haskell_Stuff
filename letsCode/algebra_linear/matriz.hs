
import Data.Array
--Matriz usando Array
-- Matriz 3x3 => inicio(i,j), fim(i,j)
matrix = array ((1,1),(3,3))
              [((1,1), (1)), ((1,2), (2)), ((1,3), (3)),
               ((2,1), (4)), ((2,2), (5)), ((2,3), (6)),
               ((3,1), (7)), ((3,2), (8)), ((3,3), (9))
              ]

-- array :: Ix i => (i, i) -> [(i, e)] -> Array i e
-- elems :: Array i e -> [e]
--Implementação que percorre o array retornando seus elementos
ele_mtx' = [matrix ! (i,j) | i<-[n..m], j<-[n..m]]
             where  n = fst $ (.) fst bounds matrix
                    m = snd $ (.) snd bounds matrix
-- elems: retorna os elementos do array
ele_mtx = elems matrix

--Matriz com Listas
matriz = [[1,2,3],
          [4,5,6],
          [7,8,9]]

-- Percorre a matriz e retona seus elementos
ele_mtz = [ n | m <-matriz, n <- m]

-- Recebe Linha e Coluna e retorna o elemento em l c
get_ele_mtz l c = (!!) (matriz !! l) c

main = do
        putStrLn "Matriz com Array"
        print matrix
        print $ bounds matrix
        print $ matrix ! (1,1)
        print $ matrix ! (2,2)
        print ele_mtx
        print ele_mtx'
        putStrLn "Matriz com Listas"
        print matriz
        print ele_mtz
        print $ length matriz
        print $ matriz !! 0
        print $ matriz !! 2
        print $ get_ele_mtz 0 0
        print $ get_ele_mtz 2 2
