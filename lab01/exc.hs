-- Gabriel de França Marques - 42107490
-- TODOS os exercícios foram realizados num RaspberryPi4 rodando Raspbian e compilados com o ghci, por isso não há a necessidade de usar uma main como seria no replit. A IDE sendo usada é o vim, sendo utilizado pelo Windows Terminal via SSH.
-- só preciso descobrir como trocar a cor dos comentários, tá tudo AZUL
-- edição 15 segundos depois: achei :)

-- Exercício 1: Escreva em Haskell o programa tot3, que recebe uma lista e totaliza a cada 3 elementos da lista.
tot3 :: [Int] -> [Int]
tot3 [] = []
tot3 xs = sum (take 3 xs) : tot3 (drop 3 xs)

-- Exercício 2: Escreva em Haskell o programa rev, que inverte a lista.
rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) = rev xs ++ [x]


-- Exercício 3: Escreva em Haskell o programa ult, que devolve o último elemento de uma lista não vazia
ult :: [Int] -> Int
ult [a] = a
ult (_:xs) = ult xs

-- Exercício 4: Escreva o penult, que recebe uma lista com pelo menos 2 elementos e devolve como resposta o penúltimo elemento da lista.
penult :: [a] -> a
penult [a,_] = a
penult (_:xs) = penult xs

-- Exercício 5: Escreva o seg, que recebe uma lista com pelo menos 2 elementos e devolve o segundo elemento da lista.
seg :: [a] -> a
seg (_:x:_) = x

-- Exercício 6: Escreva o del_rep, que recebe uma lista e deleta repetidos desta lista.
del_rep :: Eq a => [a] -> [a]
del_rep [] = []
del_rep (x:xs) = x : del_rep (filter (/= x) xs)
-- A iteração será de x a xs. Filter eliminará todos as vezes que x aparecer repetido na lista xs. /= verifica se o eleemento é diferente.

-- Exercício 7: Escreva o totk, que recebe uma lista e um natural k e totaliza a lista de k em k elementos.
totk :: Int -> [Int] -> [Int]
totk _ [] = []
totk x xs = sum (take x xs) : totk x (drop x xs)

-- Exercício 8: Escreva o trok2, que recebe uma lista e troca de lugar o primeiro com o segundo elemento, o terceiro com o quarto e assim por diante.
trok2 :: [Int] -> [Int]
trok2 [x] = [x]
trok2 (x:b:xs) = b : x : trok2 xs

-- Exercício 9: Escreva o delk, que recebe uma lista e um natural k e deleta um a cada k elementos da lista
delk :: Int -> [a] -> [a]
delk _ [] = []  -- Tratamento para lista vazia
delk k xs = delk' k xs 1
  where
    delk' _ [] _ = []  -- Caso base
    delk' k (x:xs) n
      | n `mod` k == 0 = delk' k xs (n + 1) -- Deixa de fora o elemento a cada k
      | otherwise = x : delk' k xs (n + 1)   -- Mantém o elemento caso contrário

