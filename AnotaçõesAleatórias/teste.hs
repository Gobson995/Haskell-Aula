{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
f x = 2 * x + 1

g x y = 2 * x + y

maior a b = if a>b then a else b

maior3 a b c = if a>b then if a>c then a else c else if b>c then b else c 

maior3 a b c = maior (maior a b) c