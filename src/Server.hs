{-|
Module      : Server
Description : This module represents a game server for connect four network game.

Module provides an only function that starts gameserver. Server makes two connections with players, and then asynchronously listens on both. If any of the players sends data to server, it is proccessed in a main thread and response is returned to the sender via same conncetion.
-}
module Server(
    runServer
) where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Fix (fix)
import GameLogics
import Text.Read(readMaybe)
import Data.Maybe

type Msg = (Player, String)
type GameState = StateT Position IO

-- | Main function of the module. Listens on given address for two players (first connected is given red team to play for, second - yellow) and then starts game loop.
runServer :: String -- ^ port to listen on for players
          -> IO()
runServer port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
    bind sock $ addrAddress $ head addrinfos
    listen sock 2
    chan <- newChan
    (sockR, _ ) <- accept sock
    hdlR <- getHandle sockR
    tidR <- forkIO (runClient hdlR R chan)
    (sockY, _ ) <- accept sock
    hdlY <- getHandle sockY
    tidY <- forkIO (runClient hdlY Y chan)
    let playersdict = handleForPlayer [(R, hdlR), (Y, hdlY)]
    execStateT (serverLoop chan playersdict) newGame
    tellAll playersdict "Exit"
    handle (\(SomeException _) -> return ()) $ do
                                                killThread tidR
                                                hClose hdlR
                                                close sockR
    handle (\(SomeException _) -> return ()) $ do
                                                killThread tidY
                                                hClose hdlY
                                                close sockY

serverLoop :: Chan Msg -> (Player -> Handle) -> GameState ()
serverLoop chan h4p = do
    pos <- get
    lift $ putStrLn "Current position:"
    lift $ print pos
    case evaluatePosition pos of
        Win R -> lift $ tellWin R  h4p pos
        Win Y -> lift $ tellWin Y h4p pos
        Draw -> lift $ tellDraw h4p pos
        InProgress -> do
            let hdl = h4p $ turn pos
            lift $ hPrint hdl $ board pos
            f <- gameIteration chan h4p
            when f $ serverLoop chan h4p

gameIteration :: Chan Msg -> (Player -> Handle) -> GameState Bool
gameIteration chan h4p = do
    pos <- get
    (p, msg) <- lift $ readChan chan
    let move = fromMaybe (-1) (readMaybe msg)
        hdl = h4p p
    if msg == "disconnect"
        then
            return False 
        else
            if p /= turn pos
                then
                    lift (hPutStrLn hdl "Not your turn!") >> gameIteration chan h4p
                else
                    if move `notElem` legalMoves (board pos)
                        then
                            lift (hPutStrLn hdl "Not legal move!") >> gameIteration chan h4p
                        else
                            modify' (makeMove move) >> return True 

runClient :: Handle -> Player -> Chan Msg -> IO()
runClient hdl p chan = do
    hPrint hdl p
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- hGetLine hdl
        writeChan chan (p, line)
        loop
    putStrLn $ "Player " ++ show p ++ " disconnected"
    writeChan chan (p, "disconnect")

getHandle :: Socket -> IO Handle
getHandle sock = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    return hdl

handleForPlayer :: [(Player, Handle)] -> Player -> Handle
handleForPlayer hdlsMap p = snd $ head $ filter ((== p) . fst) hdlsMap

tellWin :: Player -> (Player -> Handle) -> Position -> IO ()
tellWin p h4p pos = do
    tellAll h4p (show pos)
    tellAll h4p ((if p == R then "Red" else "Yellow") ++ " wins\n")

tellDraw :: (Player -> Handle) -> Position -> IO ()
tellDraw h4p pos = do
    tellAll h4p (show pos)
    tellAll h4p "Draw"

tellAll :: (Player -> Handle) -> String -> IO ()
tellAll h4p msg = do
    catch (hPutStrLn (h4p R) msg) (\(SomeException _) -> return ())
    catch (hPutStrLn (h4p Y) msg) (\(SomeException _) -> return ())
    putStrLn msg
