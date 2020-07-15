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

sieve (x:xs) = x : (sieve [y | y <- xs, y `mod` x /= 0])

primes :: () -> [Int]   
primes () = sieve [2,3..]