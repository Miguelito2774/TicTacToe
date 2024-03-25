module TicTacToe where

import Util (surround, nth)
import Data.List (transpose)
import Data.Foldable (asum)
import Player

instance Show Tile where
  show X = "X"
  show O = "O"
  show Empty = " "

type Board = [[Tile]]

showBoard :: Board -> String
showBoard xss = unlines
  . surround vtx
  . map (concat . surround mid . map show)
  $ xss
  where
    mid = "|"
    vtx = surround '+' $ replicate (length (head xss)) '-'

whoWon :: Board -> Maybe Player
whoWon xss = asum
            . map winner
            $ diag : anti : cols ++ xss
  where
    cols = transpose xss
    diag = zipWith (!!) xss [0..]
    anti = zipWith (!!) (reverse xss) [0..]

    winner (x:xs) = if all (==x) xs && x /= Empty
                    then Just (toPlayer x)
                    else Nothing

getCoords:: Int -> Board -> (Int, Int)
getCoords n = divMod (n-1) . length

fillTile:: Board -> Int -> Tile -> Board
fillTile xss n title = nth row (nth col (const title)) xss
  where
    (row, col) = getCoords n xss


isOver:: Board -> Bool
isOver = all (notElem Empty)
