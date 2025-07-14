

tamanho (x:xs) = 1 + length xs

pertence n [] = False
pertence n (x:xs) = if n == x then True else pertence n xs

intercessao [] _ = []
intercessao (x:xs) ys = if pertence x ys then x:intercessao xs ys else intercessao xs ys

inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

inversa :: [a] -> [a]
inversa = foldl (flip (:)) []


np 0 _ = []
np n (x:xs) = x:np (n-1) xs
nUltimos n xs = inverso (np n (inverso xs))



soma2 [] _ = []
soma2 _ [] = []
soma2 (x:xs) (y:ys) = (x+y):soma2 xs ys

juntar ys =  zip [0..] ys

pot2a 0 = []
pot2a n = 2^n:pot2a (n-1)
pot2 n = inverso (pot2a n)

ordem [] ys = ys
ordem xs [] = xs
ordem (x:xs) (y:ys) = if x>=y then y:ordem (x:xs) ys else x:ordem xs (y:ys)

comparar n [] = n
comparar n (x:xs) = if n<=x then comparar n xs else comparar x xs
menor (x:xs) = comparar x xs

removerElem 0 xs = xs
removerElem n (x:xs) = if n == x then removerElem (n-n) xs else x:removerElem n xs

ins n [] = []
ins n (x:xs) = if n>x then x:ins n xs else if n==x then x:ins n xs else n:x:xs

enesimo 1 (x:xs) = x
enesimo n (x:xs) = enesimo (n-1) xs

repetir 0 e = []
repetir n e = e:repetir (n-1) e

maiorr = filter (>2)

maiorrr p [] = []
maiorrr p (x:xs) = if p x then x:maiorrr p xs else maiorrr p xs

meuFoldr f b [] = b
meuFoldr f b (x:xs) = f x (meuFoldr f b xs)