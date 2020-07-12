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


primes = 2:(minus [3..] composites)
  where
    composites = union [multiples p | p <- primes]

multiples n = map (n*) [n..]

minus (x:xs) (y:ys) | x < y = x:(minus xs (y:ys))
                      | x == y = minus xs ys
                      | x > y =  minus (x:xs) ys
union = foldr merge []
  where
    merge (x:xs) ys = x:merge' xs ys
    merge' (x:xs) (y:ys) | x < y = x:merge' xs (y:ys)
                         | x == y = x:merge' xs ys
                         | x > y = y:merge' (x:xs) ys
--primes = sieve [2..]
--  where
--    sieve (x:xs) = x : sieve ( xs `minus` [x*x, x*x+x..])

--minus (x:xs) (y:ys) = case (compare x y) of 
--           LT -> x : minus  xs  (y:ys)
--           EQ ->     minus  xs     ys 
--           GT ->     minus (x:xs)  ys
--minus  xs     _     = xs