module Main where

largestFactorFrom f n
    | f == n         = f
    | n `mod` f == 0 = largestFactorFrom f (n `div` f)
    | otherwise      = largestFactorFrom (f+1) n

largestPrimeFactor = largestFactorFrom 2

solution = largestPrimeFactor 600851475143

main = print solution

