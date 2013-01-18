module Main where

palindromic n = show n == reverse (show n)

threeDigitNumbers = [100..999]

products = do
    x <- threeDigitNumbers
    y <- threeDigitNumbers
    return $ x * y

solution :: Integer
solution = maximum . filter palindromic $ products

main = print solution

