{-|
Module      : HumanPlayer
Description : This module represents human player in connect four game.

Module enables user to connect to a gameserver from a terminal window and play game by typing moves in. Moves are represented as number of column from 0 to 6 to put player's token in. 
-}
module HumanPlayer(runClient) where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad.Fix (fix)
import Control.Monad (forever, when)
import GameLogics
import Data.List
import Text.Read (readMaybe)

-- | Main function of the module. Connects to the game server, then starts two threads: one for redirecting standard input to game server, second for displaying data coming from game server on standard output. 
runClient :: String -- ^ address of game server
          -> String -- ^ ort of game server to connect to
          -> IO()
runClient host port = do
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serveraddr)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    writertid <- forkIO $ fix $ \loop -> do
        line <- getLine
        hPutStrLn hdl line
        loop
    clientLoop hdl
    killThread writertid
    hClose hdl
    close sock

parseInput :: String -> IO Bool
parseInput "R" = putStrLn "You are playing Red!" >> return True
parseInput "Y" = putStrLn "You are playing Yellow!" >> return True
parseInput "Exit" = return False
parseInput s = case (readMaybe s :: Maybe Board) of
                Just b -> do
                    putStrLn "Your move!"
                    putStrLn $ boardToString b
                    return True
                Nothing -> putStrLn s >> return True

clientLoop :: Handle -> IO ()
clientLoop hdl = do
    line <- hGetLine hdl
    flag <- parseInput line
    when flag $ clientLoop hdl