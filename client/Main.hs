module Main where

import System.Environment
import HumanPlayer

parseArgs :: [String] -> IO()
parseArgs [host, port] = runClient host port
parseArgs _ = putStrLn "Usage: Client-exe host port"

main :: IO ()
main = getArgs >>= parseArgs