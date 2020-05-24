module GameLogics(
    Player(..),
    Board,
    Result(..),
    Position(..),
    newGame,
    makeMove,
    legalMoves,
    evaluate
) where

import Data.List

data Player = R | Y deriving (Show, Eq)
type Board = [[Player]]
data Result = Win Player | Draw | InProgress deriving (Show, Eq)
data Position = Position {
    turn :: Player,
    board :: Board
} deriving Show

newGame :: Position
newGame = Position R $ replicate 7 []

makeMove :: Position -> Int -> Position
makeMove (Position turn board) col = Position (reverseTurn turn) (addDisc board)
    where
        reverseTurn :: Player -> Player
        reverseTurn R = Y
        reverseTurn Y = R
        addDisc :: Board -> Board
        addDisc board = zipWith (\ column index -> if index == col then turn:column else column) board [0..]

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

evaluate :: Position -> Result
evaluate (Position p b)
    | checkBoard R b = Win R
    | checkBoard Y b = Win Y
    | null $ legalMoves b = Draw
    | otherwise = InProgress