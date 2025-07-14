somatorio [] = 0
somatorio (x:xs) = x + somatorio xs

produtorio [] = 1
produtorio (x:xs) = x * produtorio xs

meuFoldr f b [] = b
meuFoldr f b (x:xs) = f x (meuFoldr f b xs)