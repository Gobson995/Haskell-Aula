nprimeiros 0 (x:xs) = []
nprimeiros n (x:xs) = x:nprimeiros (n-1) xs

inverso [] = []
inverso (x:xs) = inverso xs ++ [x]
ultimo (x:xs) = head (inverso xs)

replicara 0 x = []
replicara n x = x:replicara (n-1) x
replicar n [] = []
replicar n (x:xs) = replicara n x ++ replicar n xs

fatiar a b [] = []
fatiar a (-1) (x:xs) = []
fatiar a b (x:xs) = if a == 0 then x:fatiar a (b-1) xs else fatiar (a-1) (b-1) xs







primeiro (x,y) = x
segundo (x,y) = y
menores [] = []
menores (x:xs) = if primeiro x < segundo x then x:menores xs else menores xs
