module Main (main) where

import Game
import Player

main :: IO ()
main = gameStep Player1 (starBoard 3)
