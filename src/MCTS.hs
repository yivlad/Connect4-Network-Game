{-|
Module      : MCTS
Description : This module defines a way to find a best move in a current postion.

Module represents Haskell implementation of Monte Carlo Tree Search. Implemented MCTS has classic configuration: after adding node to a tree only one rollout is executed, selection is done by chosing any non-visited node, or, if all are visited, by using UCB formula.
-}
module MCTS(doMCTS) where

import System.Random
import Data.List
import Data.Ord
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import GameLogics

data MCT = MCT{
    pos :: Position,
    children :: [MCT],
    rWins :: Int,
    yWins :: Int,
    totalPlays :: Int
}

newNode :: Position -> MCT
newNode p = MCT p [] 0 0 0

expandNode :: MCT -> MCT
expandNode m = m{children = map (expandNode . newNode . flip makeMove p) l}
    where
        p = pos m
        l = legalMoves $ board p

rollOut :: StdGen -> Position -> (Result, StdGen)
rollOut gen p = case evaluatePosition p of
    InProgress -> rollOut gen' p'
    a -> (a, gen)
    where
        l = legalMoves $ board p
        (i, gen') = randomR (0,length l-1) gen
        p' = makeMove (l !! i) p

mcts :: StdGen -> MCT -> (MCT, Result, StdGen)
mcts gen m
    | s /= InProgress = (backPropagation m s, s, gen)
    | totalPlays m == 0 = (backPropagation m r, r, gen')
    | otherwise = (modifiedNode, resChild, gen''')
    where
        s = evaluatePosition $ pos m
        (r, gen') = rollOut gen $ pos m
        (selectedIndex, gen'') = selectNode gen m
        (selectedChild, rest) = extract selectedIndex $ children m
        (modifiedChild, resChild, gen''') = mcts gen'' selectedChild
        modifiedNode = backPropagation m{children = modifiedChild:rest} resChild

backPropagation :: MCT -> Result -> MCT
backPropagation m (Win R) = MCT (pos m) (children m) (rWins m + 1) (yWins m) (totalPlays m + 1)
backPropagation m (Win Y) = MCT (pos m) (children m) (rWins m) (yWins m + 1) (totalPlays m + 1)
backPropagation m Draw = MCT (pos m) (children m) (rWins m) (yWins m) (totalPlays m + 1)
backPropagation m InProgress = m

selectNode :: StdGen -> MCT -> (Int, StdGen)
selectNode gen m
    | not $ null unvisited = (snd $ unvisited !! i, gen')
    | otherwise = (best, gen)
    where
        chld = children m
        unvisited = filter ((== 0) . totalPlays . fst) (zip chld [0..])
        (i, gen') = randomR (0,length unvisited - 1) gen
        player = turn $ pos m
        nBig = fromIntegral $ totalPlays m
        ucbs = map ucb chld
        best = snd $ maximumBy (comparing fst) (zip ucbs [0..])
        ucb :: MCT -> Double
        ucb mc = (w / nSmall) + c * sqrt (log nBig / nSmall)
            where
                w = fromIntegral $ if player == R then rWins mc else yWins mc
                nSmall = fromIntegral $ totalPlays mc
                c = 1.414213

extract :: Int -> [a] -> (a, [a])
extract 0 (x:xs) = (x,xs)
extract n (x:xs) = (y, x:ys)
  where (y,ys) = extract (n-1) xs

bestChild :: MCT -> MCT
bestChild m = maximumBy cmpMCT $ children m
    where
        player = turn $ pos m
        winrate :: MCT -> Double
        winrate mc = fromIntegral (if player == R then rWins mc else yWins mc) / fromIntegral (totalPlays mc)
        cmpMCT :: MCT -> MCT -> Ordering
        cmpMCT m1 m2 = compare (winrate m1) (winrate m2)

getBestMove :: MCT -> Int
getBestMove m = fromMaybe (-1) t
    where
        bb = board $ pos $ bestChild m
        cb = board $ pos m
        t = elemIndex False $ zipWith (==) bb cb

-- | Main function of the module - evaluates position using Monte Carlo Tree Serach for a given number of milliseconds.
doMCTS :: StdGen -- ^ StdGen used for randomness
       -> Position -- ^ Game position to evaluate
       -> Int -- ^ Number of milliseconds to evaluate for
       -> IO(Int, StdGen) -- ^ (number of column that represents the best found way to put token, StdGen returned from all random actions)
doMCTS gen p ms = do
    currentTimestamp <- getCurrentTime
    let maxTimeStamp = addUTCTime (fromIntegral ms / 1000) currentTimestamp
    let startTree = expandNode $ newNode p
    (endTree, gen') <- mctsLoop startTree maxTimeStamp gen
    return (getBestMove endTree, gen')
    where
      mctsLoop m maxTimeStamp g = do
                  currentTimestamp <- getCurrentTime
                  if currentTimestamp < maxTimeStamp then do
                    let (m', _, gen') = mcts gen m
                    ((mctsLoop $! m') $! maxTimeStamp) $! gen'
                  else
                    return (m, g)