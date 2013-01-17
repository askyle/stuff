module Main where

x `divides` y = y `mod` x == 0

primes = 2 : filter isPrime [3..]

none p = not . any p

isPrime x = none (`divides` x) . filter (\y -> y*y <= x) $ primes

primeFactors x = filter (`divides` x) . takeWhile (<= x) $ primes

largestPrimeFactor = last . primeFactors

solution = largestPrimeFactor 600851475143

main = print solution

