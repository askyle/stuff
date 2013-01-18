module Main where

primeFactorsFrom p n
    | p == n         = [p]
    | n `mod` p == 0 = p : primeFactorsFrom p (n `div` p)
    | otherwise      = primeFactorsFrom (p+1) n

primeFactors 1 = [1]
primeFactors n = 1 : primeFactorsFrom 2 n

mergeWithMultiplicity xs []         = xs
mergeWithMultiplicity [] ys         = ys
mergeWithMultiplicity (x:xs) (y:ys)
    | x == y    = x : mergeWithMultiplicity xs ys
    | x <  y    = x : mergeWithMultiplicity xs (y:ys)
    | otherwise = y : mergeWithMultiplicity (x:xs) ys

solution = product . foldr mergeWithMultiplicity [] . map primeFactors $ [1..20]

main = print solution

