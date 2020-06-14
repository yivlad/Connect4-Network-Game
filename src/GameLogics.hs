{-|
Module      : GameLogics
Description : This module provides a basic set of rules to define connect four game.

Module defines all types and functions needed to store and manipulate gamestate. These functions are used in both Server and MCTS modules to define possible continuation of the current game or state in which the game has ended. 
-}
module GameLogics(
    Player(..),
    Board,
    Result(..),
    Position(..),
    newGame,
    makeMove,
    legalMoves,
    evaluatePosition, 
    boardToString
) where

import Data.List

-- | Players : R - Red, Y - Yellow. Red one starts the game.
data Player = R | Y deriving (Show, Eq, Read)

-- | Current state of the game - every column is described as stack of players' tokens. 
type Board = [[Player]]

-- | Result of the game.
data Result = Win Player | Draw | InProgress deriving (Show, Eq)

-- | Position is a wrapper for board type, so it is possible to get player whose current turn it is without counting tokens on board.
data Position = Position {
    turn :: Player,
    board :: Board
}

instance Show Position where
    show pos = boardToString $ board pos

-- | User-friendly view of board.
boardToString :: Board -> String
boardToString b = intercalate "\n" $ transpose $ map (\c -> replicate (6 - length c) '#' ++ concatMap show c) b

-- | Creates new game with zero tokens and Red's turn.
newGame :: Position
newGame = Position R $ replicate 7 []

-- | Adds token to a specified column and changes turn.
makeMove :: Int -- ^ number of column to put token in
         -> Position -- ^ start postion
         -> Position -- ^ position after move
makeMove col (Position turn board) = Position (reverseTurn turn) (addDisc board)
    where
        reverseTurn :: Player -> Player
        reverseTurn R = Y
        reverseTurn Y = R
        addDisc :: Board -> Board
        addDisc board = zipWith (\ column index -> if index == col then turn:column else column) board [0..]

-- | Takes board and returns all numbers of columns where it is possible to put token.
legalMoves :: Board -> [Int]
legalMoves board = map snd $ filter ( (< 6) . length . fst) $ zip board [0..]

checkColumn :: Player -> [Player] -> Bool
checkColumn player column = snd longestSeq > 3
    where
        longestSeq = foldl' longestSeqRec (0, 0) column
        longestSeqRec (c, m) p = let n = if p == player then c + 1 else 0 in (n, max n m)

checkColumns :: Player -> Board -> Bool
checkColumns p = any (checkColumn p)

checkRows :: Player -> Board -> Bool
checkRows p b = or $ do
                    x <- [0..3]
                    y <- [0..6]
                    return $ all (\n -> checkTile p b (x + n) y) [0..3]

checkDiagonals :: Player -> Board -> Bool
checkDiagonals p b = or $ do
                    x <- [0..3]
                    y <- [0..2]
                    return $ all (\n -> checkTile p b (x + n) (y + n)) [0..3]

checkBoard :: Player -> Board -> Bool
checkBoard p b = checkColumns p b || checkRows p b || checkDiagonals p b || checkDiagonals p (reverse b)

checkTile :: Player -> Board -> Int -> Int -> Bool
checkTile p b x y
    | y > l = False
    | otherwise = (b !! x !! (l - y)) == p
    where
        l = length (b !! x) - 1

-- | Takes position and returns whether game has ended and if so, which result it had.
evaluatePosition :: Position -> Result
evaluatePosition (Position p b)
    | checkBoard R b = Win R
    | checkBoard Y b = Win Y
    | null $ legalMoves b = Draw
    | otherwise = InProgress