module ComputerPlayer where

import Network.Socket
import System.IO
import System.Random
import GameLogics
import Text.Read (readMaybe)
import Control.Monad
import MCTS

data Command = Ignore | Exit | FindMove Position | Unknown

runAIPlayer :: IO ()
runAIPlayer = do
    addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "8888")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serveraddr)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    mainLoop hdl (mkStdGen 123)
    hClose hdl
    close sock

mainLoop :: Handle -> StdGen ->IO ()
mainLoop hdl gen = do
    line <- hGetLine hdl
    command <- parseInput line
    (f, gen') <- parseCommand command hdl gen
    when f (mainLoop hdl gen')

parseInput :: String -> IO Command
parseInput "R" = return Ignore
parseInput "Y" = return Ignore
parseInput "Exit" = return Exit
parseInput s = case (readMaybe s :: Maybe Board) of
                Just b -> return $ FindMove $ boardToPosition b
                Nothing -> return Unknown

boardToPosition :: Board -> Position
boardToPosition b = Position t b
    where
        t = if odd (sum $ map length b) then Y else R

parseCommand :: Command -> Handle -> StdGen  -> IO (Bool, StdGen)
parseCommand Exit _ gen = return (False, gen)
parseCommand (FindMove p) hdl gen =
                        if evaluatePosition p == InProgress then
                            do
                                (response, gen') <- doMCTS gen p 2000
                                hPrint hdl response
                                return (True, gen')
                        else
                            return (True, gen)
parseCommand _ _ gen = return (True, gen)
