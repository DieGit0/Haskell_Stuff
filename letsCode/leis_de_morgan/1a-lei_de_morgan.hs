module A_1a_lei_morgan where
  
-- ** Lógica Formal **
-- 1a) Lei de Morgan:
-- A negação da disjunção é a conjunção das negações∷
-- Forma Computacional∷ Not (P ^ Q) = Not P v Not Q

p = True
q = False
a = not (p && q)
b = not p || not q

main = do
  print a
  print b
  print (a == b)
