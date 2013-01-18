module Main where

square x = x * x

sumOfSquares = sum . map square

squareOfSum = square . sum

diff xs = squareOfSum xs - sumOfSquares xs

solution = diff [1..100]

main = print solution

