compDuplas [] = []
compDuplas ((x,y):xs) = (y,x):compDuplas xs

zipa xs [] = []
zipa [] ys = []
zipa (x:xs) (y:ys) = (x,y): zipa xs ys

semVogal = map (filter (`notElem` "aeiouAEIOU"))