
-- Implementação de Modus Tollens => Prova Indireta (inferência da Premissa pelo Consequente)
--  Se P então Q :: P -> Q
--  Não Q -> Não P

-- Implementação do Modus Tollens
mt True  thenP elseNotP = thenP
mt False thenP elseNotP = elseNotP

main = do
       let q = True
       print $ mt (q) (let p=True in p) (let p=False in p)
       let q = False
       print $ mt (q) (let p=True in p) (let p=False in p)
    -- Mais didático:
       let q = True
       print $ mt (q == True) "P" "not P"
       let q = False
       print $ mt (q == True) "P" "not P"
