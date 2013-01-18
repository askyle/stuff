module Main where

import Control.Monad

specialTriplets = do
    c <- [1..1000]
    b <- [1..c-1]
    let a = 1000 - (b+c)
    guard $ a > 0 && a < b
    return (a,b,c)

pythagorean (a,b,c) = a*a + b*b == c*c

specialPythagoreans = filter pythagorean specialTriplets

solution = a*b*c where (a,b,c) = head specialPythagoreans

main = print solution


