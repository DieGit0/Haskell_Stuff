module C_modus_ponens where
--  Modus Ponens => inferência do Consequente pela Premissa)
--  Se  P então Q :: P -> Q
--  Não P -> Não Q

-- Implementação do Modus Ponens
mp True  thenQ elseNotQ = thenQ
mp False thenQ elseNotQ = elseNotQ

--let p = undefined
--    q = undefined

main = do
       let p = True
       print $ mp (p) (let q=True in q) (let q=False in q)
       let p = False
       print $ mp (p) (let q=True in q) (let q=False in q)
    -- Mais didático:
       let p = True
       print $ mp (p == True) "Q" "not Q"
       let p = False
       print $ mp (p == True) "Q" "not Q"
