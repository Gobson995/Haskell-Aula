data Bool = Trie | False

data Booleano = Verdade | Falso deriving Show

Verdade &&& Verdade = Verdade
_ &&& _ = Falso

Falso ||| Falso = Falso
_ ||| _ = Verdade

data Semana = Dom | Seg | Ter | Qua | Qui | Sex | Sab deriving (Show, Eq, Ord)
proxDiaUtil Seg = Ter
proxDiaUtil Ter = Qua
proxDiaUtil Qua = Qui
proxDiaUtil Qui = Sex
proxDiaUtil _ = Seg

ehDiaUtil d = d > Dom && d < Sab


data Dupla a b = Par a b deriving Show

meuZip :: [a] -> [b] -> [Dupla a b]
meuZip _ [] = []
meuZip [] _ = []
meuZip (x:xs) (y:ys) = Par x y : meuZip xs ys