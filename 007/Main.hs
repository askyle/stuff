module Main where

primes = 2 : filter isPrime [2..]

isPrime n = none (`divides` n) . takeWhile (\p -> p * p <= n) $ primes

p `divides` n = n `mod` p == 0

none p = not . any p

solution = primes !! 10001

main = print solution

