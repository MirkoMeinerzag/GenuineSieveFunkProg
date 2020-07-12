import System.Environment
import System.CPUTime
import Data.Map (Map)
import qualified Data.Map as Map

main = do
    start <- getCPUTime
    let l1 = primes !! 1000
    print(l1)
    finish <- getCPUTime
    print(finish - start)
    start <- getCPUTime
    let l2 = primes !! 10000
    print(l2)
    finish <- getCPUTime
    print(finish - start)
    start <- getCPUTime
    let l3 = primes !! 100000
    print(l3)
    finish <- getCPUTime
    print(finish - start)
    start <- getCPUTime
    let l4 = primes !! 1000000
    print(l4)
    finish <- getCPUTime
    print(finish - start)
    start <- getCPUTime
    let l5 = primes !! 10000000
    print(l5)
    finish <- getCPUTime
    print(finish - start)
    
primes = sieveMap [2..]
sieveMap xs = sieve' xs Map.empty
  where
    sieve' [] table = []
    sieve' (x:xs) table =
      case Map.lookup x table of
        Nothing -> x : sieve' xs (Map.insert (x*x) [x] table)
        Just facts -> sieve' xs (foldl reinsert (Map.delete x table) facts)
      where
        reinsert table prime = Map.insertWith (++) (x+prime) [prime] table