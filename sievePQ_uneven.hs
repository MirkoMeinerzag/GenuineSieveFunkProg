import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as MinPQueue
import System.Environment
import System.CPUTime
import Control.DeepSeq

main = do
    start <- getCPUTime
    let l1 = (primes ()) !! 1000
    finish <- deepseq l1 getCPUTime
    print("CPU runtime for 1000 primes in picoseconds:")
    print(finish - start)
    start <- getCPUTime
    let l2 = (primes ()) !! 10000
    finish <- deepseq l2 getCPUTime
    print("CPU runtime for 10000 primes in picoseconds:")
    print(finish - start)
    start <- getCPUTime
    let l3 = (primes ()) !! 100000
    finish <- deepseq l3 getCPUTime
    print("CPU runtime for 100000 primes in picoseconds:")
    print(finish - start)
    start <- getCPUTime
    let l4 = (primes ()) !! 1000000
    finish <- deepseq l4 getCPUTime
    print("CPU runtime for 1000000 primes in picoseconds:")
    print(finish - start)
    start <- getCPUTime
    let l5 = (primes ()) !! 10000000
    finish <- deepseq l5 getCPUTime
    print("CPU runtime for 10000000 primes in picoseconds:")
    print(finish - start)

primes :: () -> [Int]
primes = 2: sieve [3,5..]

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