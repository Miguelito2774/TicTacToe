module Player where

data Player = Player1 | Player2 deriving (Eq)
data Tile = X | O | Empty deriving (Eq)

instance Show Player where
  show Player1 = "Player 1"
  show Player2 = "Player 2"

toPlayer :: Tile -> Player
toPlayer X = Player1
toPlayer O = Player2

fromPlayer :: Player -> Tile
fromPlayer Player1 = X
fromPlayer Player2 = O

changePlayer :: Player -> Player
changePlayer Player1 = Player2
changePlayer Player2 = Player1



  