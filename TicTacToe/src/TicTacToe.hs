module TicTacToe where

import Util (surround, nth)
import Data.List (transpose)
import Data.Foldable (asum)
import Player

instance Show Tile where
  show X = "X"
  show O = "O"
  show Empty = " "

-- Represents the current state of the Tic Tac Toe board.
type Board = [[Tile]]

--Converts the board into a visually formatted string.
showBoard :: Board -> String
showBoard xss = unlines
  . surround vtx
  . map (concat . surround mid . map show)
  $ xss
  where
    mid = "|"
    vtx = surround '+' $ replicate (length (head xss)) '-'

-- Converts a tile into a player.
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

-- Converts a number into a pair of coordinates.
getCoords:: Int -> Board -> (Int, Int)
getCoords n = divMod (n-1) . length

-- Fills a tile on the board at the given position.
fillTile:: Board -> Int -> Tile -> Board
fillTile xss n title = nth row (nth col (const title)) xss
  where
    (row, col) = getCoords n xss


-- Checks if the game is over.
isOver:: Board -> Bool
isOver = all (notElem Empty)
