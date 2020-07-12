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
    
primes = 2 : [x | x <- [3..], isprime x]

isprime x = all (\p -> x `mod` p > 0) (factorsToTry x)
  where
    factorsToTry x = takeWhile (\p -> p*p <= x) primes