module Game(validateInput, askInput, gameStep, starBoard) where

import Player
import TicTacToe

-- Validates the user input for a tile placement.
-- It checks if the input is within the valid range and if the tile position is empty.
validateInput:: Board -> String -> Either String Int
validateInput xss s = case reads s of
  [(n, "")] -> check n
  _ -> Left "Only numbers are allowed"
  where
    check n
      | n < 1 || n > (length xss ^ 2) = Left "Out of range"
      | xss !! row !! col /= Empty = Left "Already filled"
      | otherwise = Right n
      where
        (row, col) = getCoords n xss

--Asks the player for input and handles the game progression accordingly.
askInput:: Player -> Board -> IO ()
askInput player board = do
  putStrLn $ showBoard board
  putStrLn $ "You turn, " ++ show player ++ "!"
  putStrLn $ "Tile number (1 - " ++ show (length board ^ 2) ++ "): "
  number <- getLine
  
  case validateInput board number of
    Left s -> putStrLn ("Invalid input: " ++ s) >> gameStep player board
    Right n -> let tile = fromPlayer player
                   next = changePlayer player

                   in gameStep next (fillTile board n tile)

--Handles the progression of the game.
gameStep:: Player -> Board -> IO ()
gameStep player board = case whoWon board of
  Just winner -> do
    putStrLn $ showBoard board
    putStrLn $ "Congratulations, " ++ show winner ++ " won !"

  Nothing | isOver board -> putStrLn "It's a draw!"
          | otherwise -> askInput player board

-- Generates a star-shaped Tic Tac Toe board of size n.
starBoard:: Int -> Board
starBoard n = replicate n (replicate n Empty)
