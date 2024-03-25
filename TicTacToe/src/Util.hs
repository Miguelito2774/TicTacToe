module Util(surround, nth) where

import Data.List(intersperse)

-- Surrounds a list with a given element.
surround:: a -> [a] -> [a]
surround x xs = x : intersperse x xs ++ [x]

-- Replaces the nth element of a list with a new value.
nth :: Int -> (a -> a) -> [a] -> [a]
nth _ _ [] = []
nth 0 f (x:xs) = f x : xs
nth n f (x:xs) = x : nth (n-1) f xs
