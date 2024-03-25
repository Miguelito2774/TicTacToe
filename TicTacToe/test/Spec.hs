import Test.HUnit
import Game
import Player
import TicTacToe

-- Player 1 Wins
testPlayer1Wins :: Test
testPlayer1Wins = TestCase $ do
    let board = [[X, X, X], [Empty, Empty, Empty], [Empty, Empty, Empty]]
    assertEqual "Player 1 should win" (Just Player1) (whoWon board)

-- Player 2 Wins
testPlayer2Wins :: Test
testPlayer2Wins = TestCase $ do
    let board = [[O, O, O], [Empty, Empty, Empty], [Empty, Empty, Empty]]
    assertEqual "Player 2 should win" (Just Player2) (whoWon board)

-- There is a draw
testDraw :: Test
testDraw = TestCase $ do
    let board = [[X, O, X], [X, O, X], [O, X, O]]
    assertBool "It should be a draw" (isOver board && whoWon board == Nothing)

-- Invalid input: out of range
testOutOfRangeInput :: Test
testOutOfRangeInput = TestCase $ do
    let board = starBoard 3
    assertEqual "Out of range input should return Left" (Left "Out of range") (validateInput board "10")

-- Invalid input: not a number
testInvalidInput :: Test
testInvalidInput = TestCase $ do
    let board = starBoard 3
    assertEqual "Invalid input should return Left" (Left "Only numbers are allowed") (validateInput board "abc")

main :: IO Counts
main = runTestTT $ TestList [testPlayer1Wins, testPlayer2Wins, testDraw, testOutOfRangeInput, testInvalidInput]
