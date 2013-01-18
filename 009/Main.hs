module Main where

triplets c = do
    b <- [1..c-1]
    a <- [1..b-1]
    return (a,b,c)

pythagorean (a,b,c) = a*a + b*b == c*c

special (a,b,c) = a+b+c == 1000
    
specialPythagoreans = filter pythagorean . filter special . concatMap triplets $ [3..]

solution = a+b+c where (a,b,c) = head specialPythagoreans

main = print solution


