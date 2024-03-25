module Util(surround, nth) where

import Data.List(intersperse)

surround:: a -> [a] -> [a]
surround x xs = x : intersperse x xs ++ [x]

nth :: Int -> (a -> a) -> [a] -> [a]
nth _ _ [] = []
nth 0 f (x:xs) = f x : xs
nth n f (x:xs) = x : nth (n-1) f xs





  