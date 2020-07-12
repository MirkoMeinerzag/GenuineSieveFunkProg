import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as MinPQueue
import System.Environment
import System.CPUTime

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

primes = 2 : 3 : 5 : 7 : sieve (spin wheel2357 11)

sieve [] = []
sieve (x:xs) = x : sieve' xs (insertprime x xs MinPQueue.empty)
  where
    insertprime p xs table = MinPQueue.insert (p*p) (map (* p) xs) table
    sieve' [] table = []
    sieve' (x:xs) table
        | nextComposite <= x = sieve' xs (adjust table)
        | otherwise = x : sieve' xs (insertprime x xs table)
          where
            (nextComposite, _) = MinPQueue.findMin table
            adjust table
                | n <= x = adjust (deleteMinAndInsert n' ns table)
                | otherwise = table
              where
                (n, n':ns) = MinPQueue.findMin table
                deleteMinAndInsert x xs queue = MinPQueue.insert x xs (MinPQueue.deleteMin queue)

wheel2357 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357

spin (x:xs) n = n : spin xs (n + x)