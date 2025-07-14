pertence n [] = False
pertence n (x:xs) = if n==x then True else pertence n xs

intercessao [] (x:xs) = []
intercessao (n:ns) xs = if pertence n xs then n:intercessao ns xs else intercessao ns xs

inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

nP 0 _ = []
nP n (x:xs) = x:nP (n-1) xs 
nUltimos n (x:xs) = inverso (nP n(inverso xs))

soma2 [] _ = []
soma2 _ [] = []
soma2 (x:xs) (y:ys) = (x+y): soma2 xs ys

pot 0 = []
pot n =  2^n:pot (n-1)
pot2 n = inverso(pot n)
