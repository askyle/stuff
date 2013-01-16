module Main where

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

diff :: (Ord a) => [a] -> [a] -> [a]
diff []     _      = []
diff xs     []     = xs
diff (x:xs) (y:ys)
    | x < y  = x : diff xs (y:ys)
    | x == y = diff xs ys
    | x > y  = diff (x:xs) ys

intersect :: (Ord a) => [a] -> [a] -> [a]
intersect []     _      = []
intersect _      []     = []
intersect (x:xs) (y:ys)
    | x < y  = intersect xs (y:ys)
    | x == y = x : intersect xs ys
    | x > y  = intersect (x:xs) ys

primes = primesFrom [2..] where
    primesFrom (x:xs) = x : primesFrom (sift x xs)
    sift n xs = xs `diff` iterate (+n) 0

isPrime 0 = False
isPrime 1 = False
isPrime n = none (\x -> n `mod` x == 0) . takeWhile (\x -> x * x <= n) $ primes

none f = not . any f

primeFibsSlow = intersect primes fibs

primeFibs = filter isPrime fibs

firstPrimeFibAbove n = head . dropWhile (<=n) $ primeFibs

main = print $ firstPrimeFibAbove 227000

