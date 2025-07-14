tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

somatorio [] = 0
somatorio (x:xs) = x + somatorio xs

produtorio [] = 1
produtorio (x:xs) = x * produtorio xs

maior [x] = x
maior (x:xs) = if x > m then x else m where m = maior xs