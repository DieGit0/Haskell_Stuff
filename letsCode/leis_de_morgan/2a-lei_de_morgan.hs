module B_2a_lei_morgan where
  
-- 2a) Lei de Morgan:
-- A negação da conjunção é a disjunção das negações
-- Forma Computacional∷ Not (P V Q) = Not P ^ Not Q

p = True
q = False
a = not (p || q)
b = not p && not q

main = do
  print a
  print b
  print (a == b)
