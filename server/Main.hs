module Main where

import System.Environment
import Server

parseArgs :: [String] -> IO()
parseArgs [port] = runServer port
parseArgs _ = putStrLn "Usage: Server-exe port"

main :: IO ()
main = getArgs >>= parseArgs
