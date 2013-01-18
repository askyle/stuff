module Main where

primes = 2 : filter isPrime [3..]

isPrime n = none (`divides` n) . takeWhile (\p -> p * p <= n) $ primes

p `divides` n = n `mod` p == 0

none p = not . any p

solution = sum . takeWhile (< 10) $ primes

main = print solution

