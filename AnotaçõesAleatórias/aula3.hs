fat 0 = 1
fat n = n *fat(n-1)

pot b 0 = 1
pot b e = b*pot b (e-1)

mdc a b = if a == b then a 
          else if a>b then mdc (a-b) b 
          else mdc a (b-a)

mdc1 a b | a == b = a 
         | a > b = mdc1 (a-b) b 
         | otherwise = mdc1 a (b-a)