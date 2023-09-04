-- T Tree F Folha N Nó
data T = F Integer | N Integer T T
  deriving Show -- o tipo algébrico de dados permite que imprima a árvore

t1 = (N 5
	(N 7 (F 2) (F 1))
	(N 8 (F 3) (F 9))
	)


somaT (F n) = n
somaT (N n te td) = n + somaT te + somaT td

incr (F val) | val > 5 = F( val)
	     | otherwise = (F (val+1))
incr (N val te td) | val > 5 = (N val (incr te) (incr td))
		   | otherwise = (N (val+1) (incr te) (incr td)

 
