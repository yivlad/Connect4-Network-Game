{-|
Module      : ComputerPlayer
Description : This module represents computer player in connect four game.

Module provides an interface for using MCTS module for playing game via network. 
-}
module ComputerPlayer(runAIPlayer) where

import Network.Socket
import System.IO
import System.Random
import GameLogics
import Text.Read (readMaybe)
import Control.Monad
import Control.Concurrent
import Control.Exception
import MCTS
import Data.Function (fix)

data MessageSource = External | Internal
type Msg = (MessageSource, String)
data Command = Ignore | Exit | FindMove Position | Unknown | SendMove Int

-- | Main function of the module. Connects to the game server, then starts game loop with invocation of MCTS AI.
runAIPlayer :: String -- ^ address of game server
            -> String -- ^ port of game server to connect to
            -> IO ()
runAIPlayer host port = do
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serveraddr)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    chan <- newChan
    forkIO $ listenFromServer hdl chan 
    mainLoop hdl chan
    hClose hdl
    close sock

mainLoop :: Handle -> Chan Msg ->IO ()
mainLoop hdl chan = do
    msg <- readChan chan
    command <- parseInput msg
    f <- parseCommand command chan hdl
    when f (mainLoop hdl chan)

parseInput :: Msg -> IO Command
parseInput (External, "R") = return Ignore
parseInput (External, "Y") = return Ignore
parseInput (External, "Exit") = return Exit
parseInput (External, s) = case (readMaybe s :: Maybe Board) of
                Just b -> return $ FindMove $ boardToPosition b
                Nothing -> return Unknown
parseInput (Internal, n)  = case (readMaybe n :: Maybe Int) of
                Just b -> return $ SendMove b
                Nothing -> return Unknown

boardToPosition :: Board -> Position
boardToPosition b = Position t b
    where
        t = if odd (sum $ map length b) then Y else R

parseCommand :: Command -> Chan Msg -> Handle -> IO Bool
parseCommand Exit _ _ = return False
parseCommand (FindMove p) chan _ =
                        if evaluatePosition p == InProgress then
                            do
                                forkIO $ startMCTS chan p
                                return True
                        else
                            return False
parseCommand (SendMove n) _ hdl = (hPrint hdl n) >> return True
parseCommand Unknown _ _ = return False
parseCommand _ _ _ = return True

listenFromServer :: Handle -> Chan Msg -> IO ()
listenFromServer hdl chan = do 
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- hGetLine hdl
        writeChan chan (External, line)
        loop
    writeChan chan (External, "Exit")

startMCTS :: Chan Msg -> Position -> IO()
startMCTS chan p = do
    gen <- newStdGen 
    (response, _) <- doMCTS gen p 3000
    writeChan chan (Internal, show response)
