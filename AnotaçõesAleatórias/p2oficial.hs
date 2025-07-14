trocar a b [] = []
trocar a b (x:xs) = if a==x then b:trocar a b xs else x:trocar a b xs

zipa [] [] = []
zipa [] ys = []
zipa xs [] = []
zipa (x:xs) (y:ys) = (x,y):zipa xs ys

semVogal = map (filter (`notElem` "aeiouAEIOU"))