{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Main where

import Prelude hiding ((+))
import qualified Prelude

class Monoid a where
    e   :: a
    (+) :: a -> a -> a

data Parity = Even | Odd deriving Eq

instance Monoid Parity where
    e = Even
    Even + Even = Even
    Odd  + Odd  = Even
    _    + _    = Odd

instance Monoid Integer where
    e = 0
    (+) = (Prelude.+)

instance (Monoid a, Monoid b) => Monoid (a,b) where
    e = (e,e)
    (x1,y1) + (x2,y2) = (x1+x2,y1+y2)


fibs :: [(Integer,Parity)]
fibs = (1,Odd) : (2,Even) : zipWith (+) fibs (tail fibs)

evenFibs = filter ((==Even) . snd) fibs

solution = sum . takeWhile (<= 4000000) . map fst $ evenFibs

main = print solution

