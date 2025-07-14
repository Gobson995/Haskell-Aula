import Data.List (sortBy, groupBy)
import Data.Function (on)


type Doc = String 
type Line = String 
type Word' = String 

-- letra A: nao precisa fazer pois ja está implementado


-- letra B: numerar as linhas do documento

numLines :: Doc -> [(Int,Line)]
numLines txt = numLines' 1 (lines txt)
numLines' n [] = []
numLines' n (x:xs) = (n,x):numLines' (n+1) xs

-- obs: sempre que tem uma quebra de linha, ele começa soma mais 1
--      o n começa no 1
--      usa uma função auxiliar numlines'


-- letra C: dividir o texto da linha em palavras unicas
-- EX: gustavo bada -> [(1,gustavo), (1,bada)]

allNumWords :: [(Int,Line)] -> [(Int,Word')]
allNumWords [] = []
allNumWords ((n,x):xs) = map (\p -> (n, p)) (words x) ++ allNumWords xs

-- n = numero da linha // x = frase da linha
-- words = faz uma lista de palavras com a frase
-- map = aplica a função para cada elemento da lista
-- lambda é uma função que coloca o n junto com a palavra, formando uma tupla
-- ++ = concatena com a proxima linha do texto


-- letra D: ordenar em ordem alfabetica as palabras do texto

sortLs :: [(Int,Word')] -> [(Int,Word')]
sortLs = sortBy compararp
compararp (_,p1) (_,p2) = compare p1 p2

-- sortBy = retorna uma lista ordenada entre dois elementos
-- uso uma função auxiliar para comparar duas palavras
-- compare = compara duas palavar (função pronta)


-- letra E: juntar as palavras iguais em uma lista com as linhas em que elas ocorrem

almalgamate :: [(Int,Word')] -> [([Int],Word')]
almalgamate xs = map agrupador (groupBy mesmap (sortLs xs))
mesmap :: (Int, Word') -> (Int, Word') -> Bool
mesmap (_, p1) (_, p2) = p1 == p2
agrupador :: [(Int,Word')] -> ([Int], Word')
agrupador xs = (map fst xs, snd (head xs))

-- groupBy = agrupa os elementos com caracteristicas em comum
-- map fst xs = pega o n das palavras iguais
-- snd (head xs) = pega a palavra em comum (nesse caso da primeira tupla)
-- mesmap = fala quanto as tuplas tem palavras iguais
-- agrupador = faz uma lista dos n que tem a mesma palavra no segundo da tupla
-- o sortLs = para deixar em ordem alfabetica de novo


-- Remover palavras com 3 ou menos letras

removep3 :: [(Int, Word')] -> [(Int, Word')]
removep3 = filter (\(_, p) -> length p > 3)

-- filter = mantem só os elementos que satisfazem a condição desejada
-- length = tamanho da palavra

-- letra F: eliminar os numeros repetidos das palavras das linhas

remover :: Eq a => [a] -> [a]
remover = foldr (\n seen -> if n `elem` seen then seen else n : seen) []

-- foldr = percorre da direita pra esquerda
-- se o numero ja esta na lista, ele ignora, se não esta, ele adiciona
-- seen = guarda o resultado parcial (aculador)
-- 'elem' = função para verificar se ja existe ou não

shorten :: [([Int], Word')] -> [([Int], Word')]
shorten xs = map (\(n, p) -> (remover n, p)) xs


makeindex :: Doc -> [([Int], Word')]
makeindex txt = shorten . almalgamate . sortLs . removep3 . allNumWords . numLines $ txt

formatIndex :: [([Int], Word')] -> String
formatIndex xs = unlines [ w ++ " - " ++ show ns | (ns, w) <- xs]

main :: IO ()
main = do
    putStr "Arquivos: "
    arq <- getLine
    txt <- readFile arq
    putStr $ formatIndex (makeindex txt)