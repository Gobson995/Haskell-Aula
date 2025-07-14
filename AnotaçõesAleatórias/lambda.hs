somar [] = []
somar (x:xs) = x+1:somar xs

dobrar [] = []
dobrar (x:xs) = 2*x:dobrar xs

inc x = x+1

map' f [] = []
map' f (x:xs) = f x:map' f xs

--(\f (x:xs) -> f x:map f xs) (1+)