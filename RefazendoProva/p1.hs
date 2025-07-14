--questão 1
nprimeiros 0 _ = []
nprimeiros n (x:xs) = x:nprimeiros (n-1) xs


--questão 2
inverso [] = []
inverso (x:xs) = inverso xs ++ [x]
ultimo (x:xs) = head (inverso xs)


--questão 3
aux 0 x = []
aux n x = x:aux (n-1) x
replicar _ [] = []
replicar n (x:xs) = aux n x++replicar n xs


--questão 4
osprimeiros _ [] = []
osprimeiros 0 _ = []
osprimeiros n (x:xs) = x:osprimeiros (n-1) xs
aprimeiro _ [] = []
aprimeiro 0 xs = xs
aprimeiro n (x:xs) = aprimeiro (n-1) xs
fatiar n m xs = aprimeiro n (osprimeiros (m+1) xs)


--questão 5
primeiro (x,y) = x
segundo (x,y) = y
menores [] = []
menores (x:xs) = if primeiro x < segundo x then x:menores xs else menores xs
