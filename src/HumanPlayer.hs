module HumanPlayer where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad.Fix (fix)
import Control.Monad (forever, when)
import GameLogics
import Data.List
import Text.Read (readMaybe)

runClient :: IO()
runClient = do
    addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "8888")
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