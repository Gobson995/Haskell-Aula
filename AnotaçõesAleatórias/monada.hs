import Control.Monad.RWS (Ap(getAp))
import Language.Haskell.TH (arithSeqE)
main = (putStr "Qual seu nome?" >> getLine) >>= (\nome -> putStr ("Ola," ++nome ++"\n"))

main' = do putStr "Arquivos"
           arq <- getLine
           txt <- readFile arq
           putStr txt