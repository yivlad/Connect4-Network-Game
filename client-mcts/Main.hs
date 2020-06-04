module Main where

import System.Environment
import ComputerPlayer

parseArgs :: [String] -> IO()
parseArgs [host, port] = runAIPlayer host port
parseArgs _ = putStrLn "Usage: Client-MCTS-exe host port"

main :: IO ()
main = getArgs >>= parseArgs