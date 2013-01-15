module Main where

import Data.List
import Data.Maybe
import GHC.Exts

data Cursor a = Between [a] [a]
              | At [a] a [a]

goRight, goLeft :: Cursor a -> Maybe (Cursor a)

goRight (Between _ [])    = Nothing
goRight (Between l (x:r)) = Just (At l x r)
goRight (At l x r)        = Just (Between (x:l) r)

goLeft (Between [] _ )   = Nothing
goLeft (Between (x:l) r) = Just (At l x r)
goLeft (At l x r)        = Just (Between l (x:r))

start :: [a] -> Cursor a
start = Between []

allCursors :: [a] -> [Cursor a]
allCursors = catMaybes . takeWhile isJust . iterate (>>= goRight) . Just . start

longestCommonPrefix :: (Eq a) => [a] -> [a] -> [a]
longestCommonPrefix = curry $ map fst . takeWhile (uncurry (==)) . uncurry zip

longestPalindromeHere :: (Eq a) => Cursor a -> Cursor a
longestPalindromeHere (Between l r) = (Between p p) where p = longestCommonPrefix l r
longestPalindromeHere (At l x r)    = (At p x p) where p = longestCommonPrefix l r

toList :: Cursor a -> [a]
toList (Between l r) = reverse l ++ r
toList (At l x r)    = reverse l ++ [x] ++ r

palindromes :: (Eq a) => [a] -> [[a]]
palindromes = map (toList . longestPalindromeHere) . allCursors

longestPalindrome = head . reverse . sortWith length . palindromes

main = getContents >>= print . longestPalindrome

