module Main where

-- Assumes inputs are sorted infinite lists without repetitions
union (x:xs) (y:ys)
    | x == y    = x : union xs ys
    | x < y     = x : union xs (y:ys)
    | otherwise = y : union (x:xs) ys

multiples n = n : map (+n) (multiples n)

solution = sum . takeWhile (< 1000) $ multiples 3 `union` multiples 5

main = print solution

