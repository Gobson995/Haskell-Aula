-- Exercício 1
ehTriangulo :: (Ord a, Num a) => a -> a -> a -> Bool
ehTriangulo a b c = a + b > c && a + c > b && b + c > a

-- Exercício 2
tipoTriangulo :: Eq a => a -> a -> a -> String
tipoTriangulo a b c | a == b && b == c = "equilatero"
                    | a == b || a == c || b == c = "isosceles"
                    | otherwise = "escaleno"

-- Exercício 3
triangulo :: (Ord a, Num a) => a -> a -> a -> String
triangulo a b c = if ehTriangulo a b c then tipoTriangulo a b c
                  else "nao eh um triangulo"

-- Exercício 4
somaPares :: Integral a => a -> a
somaPares n = if n <= 0 then 0 
              else if rem n 2 /= 0 then somaPares (n-1)
              else n + somaPares (n-2)

-- Exercício 5      
somaPot2m :: (Eq t, Floating t) => t -> t -> t
somaPot2m m 0 = m
somaPot2m m n = 2**n*m + somaPot2m m (n-1)

-- Exercício 6
primo :: Integral t => t -> Bool
primo n = primo' n (n-1)
primo' :: Integral t => t -> t -> Bool
primo' n 1 = True
primo' n d = if rem n d == 0 then False
             else primo' n (d-1)

-- Exercício 7
seriePI :: (Ord t, Fractional t) => t -> t
seriePI n = termos n 1 0 1
termos :: (Ord t, Fractional t) => t -> t -> t -> t -> t
termos n denominador soma sinal | (4/denominador) > (4/n) = termos n (denominador+2) (soma+sinal*(4/denominador)) (-sinal)
                                | otherwise = soma