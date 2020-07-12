import System.Environment
import System.CPUTime

main = do
    start <- getCPUTime
    let l1 = primes !! 100
    print(l1)
    finish <- getCPUTime
    print(finish - start)
    start <- getCPUTime
    let l2 = primes !! 1000
    print(l2)
    finish <- getCPUTime
    print(finish - start)
    start <- getCPUTime
    let l3 = primes !! 10000
    print(l3)
    finish <- getCPUTime
    print(finish - start)
    start <- getCPUTime
    let l4 = primes !! 100000
    print(l4)
    finish <- getCPUTime
    print(finish - start)

sieve (x:xs) = x : (sieve [y | y <- xs, y `mod` x /= 0])
    
primes = sieve [2,3..]