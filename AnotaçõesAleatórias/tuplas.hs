primeiro (x,_) = x
segundo (_,y) = y

procurar n [] = []
procurar n (x:xs) = if n == primeiro x then segundo x else procurar n xs

zipa _ [] = []
zipa [] _ = []
zipa (x:xs) (y:ys) = (x,y):zipa xs ys

tabuada = auxtabuada 1 1
auxtabuada 10 _ = []
auxtabuada n m = if m==9 then (n,m,n*m):auxtabuada (n+1) 1 else (n,m,n*m):auxtabuada n (m+1) 


--tabuada professor
tabuadaa = tabaux1 1
tabaux1 9 = tabaux2 9 0
tabaux1 n = tabaux2 n 1 ++ tabaux1 (n+1)
tabaux2 n1 9 = [(n1,9,n1*9)]
tabaux2 n1 n2 = (n1,n2,n1*n2): tabaux2 n1 (n2+1)

tabuada10 = [(x,y,y*x) |  x <- [1..10], y <- [1..10]]
