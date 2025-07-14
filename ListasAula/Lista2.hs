--Exercicio 1
pertence n [] = False
pertence n (x:xs) = if n==x then True else pertence n xs 

--Exercicio 2
intercessao [] (x:xs) = []
intercessao (n:ns) xs = if pertence n xs then n:intercessao ns xs else intercessao ns xs  

--Exercicio 3
inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

--exercicio 4
nU 0 _ = []
nU n (x:xs) = x:nU (n-1) xs
nI [] = []
nI (x:xs) = nI xs ++ [x]
nUltimos n (x:xs) = nI (nU n (nI xs))

--exercicio 5
soma2 _ [] = []
soma2 [] _ = []
soma2 (x:xs) (y:ys) = x+y:soma2 xs ys

--exercicio 6
pot 0 = []
pot n = 2^n:pot(n-1)
pot2 n = inverso (pot n)

--exercicio 7
intercalacao xs [] = xs 
intercalacao [] ys = ys
intercalacao (x:xs) (y:ys) =if x>=y then y:intercalacao (x:xs) ys else x:intercalacao xs (y:ys)

--exercicio 8
menor' n [] = n
menor' n (x:xs) = if n<=x then menor' n xs else menor' x xs
menor (x:xs) = menor' x xs

--exercicio 9
removerElem n [] = []
removerElem n (x:xs) = if n==x then removerElem n xs else x:removerElem n xs

--exercicio 10
ordenar [] = []
ordenar xs = m:ordenar (removerElem m xs)
                 where m = menor xs
              
--exercicio 11
ins n [] = [n]
ins n (x:xs) = if n == x then x:xs else if n<x then n:x:xs else x:ins n xs

--exercicio 12
enesimo 1 (x:xs) = x
enesimo n (x:xs) = enesimo (n-1) xs

--exercicio 13
repetir 0 x = []
repetir n x = x:repetir (n-1) x

--exercicio 14
int2Char :: Int -> Char
int2Char d = toEnum (d+48)
numString 0 = []
numString n = numString (div n 10) ++ [int2Char (rem n 10)]

--exercicio 15
aux _ (-1) = 0
aux (x:xs) b = ((char2int x)*10^b) + aux xs (b-1)
stringNum [] = 0
stringNum a = aux a ((length a)-1)
char2int :: Char -> Int
char2int xs = fromEnum (xs)-48

--exercicio 16
bin2int [] = 0
bin2int (x:xs) = ((char2int x)*2^(length (x:xs)-1)) + bin2int xs

--exercicio 17
auxiliar 0 = []
auxiliar n = (rem n 2):auxiliar(div n 2)
listaString [] = []
listaString (x:xs) = int2Char x:listaString xs
int2bin n = listaString(inverso(auxiliar n))

--exercicio 18
char2i :: Char -> Int
char2i xs = fromEnum (xs)
int2c :: Int -> Char
int2c d = toEnum d
minusculas :: [Char] -> [Char]
minusculas [] = []
minusculas (x:xs) = let m = (char2i x)in if m >=65 && m<=90 then (int2c (m+32)):minusculas xs else x:minusculas xs

