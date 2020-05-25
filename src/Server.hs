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

runServer :: String -> IO()
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
    forkIO (runClient hdlR R chan)
    (sockY, _ ) <- accept sock
    hdlY <- getHandle sockY
    forkIO (runClient hdlY Y chan)
    execStateT (serverLoop chan $ handleForPlayer [(R, hdlR), (Y, hdlY)]) newGame
    return ()

serverLoop :: Chan Msg -> (Player -> Handle) -> GameState ()
serverLoop chan h4p = do
    pos <- get
    case evaluatePosition pos of
        Win R -> tellWin R h4p
        Win Y -> tellWin Y h4p
        Draw -> tellDraw h4p
        InProgress -> gameIteration chan h4p >> serverLoop chan h4p

gameIteration :: Chan Msg -> (Player -> Handle) -> GameState ()
gameIteration chan h4p = do
    pos <- get
    let hdlV = h4p $ turn pos
    lift $ hPrint hdlV $ board pos
    (p, msg) <- lift $ readChan chan
    let move = fromMaybe (-1) (readMaybe msg)
        hdl = h4p p
    if p /= turn pos
        then
            lift $ hPutStrLn hdl "Not your turn!"
        else
            if move `notElem` legalMoves (board pos)
                then
                    lift $ hPutStrLn hdl "Not legal move!"
                else
                    modify' (makeMove move)

runClient :: Handle -> Player -> Chan Msg -> IO()
runClient hdl p chan = do
    hPrint hdl p
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        writeChan chan (p, line)

getHandle :: Socket -> IO Handle
getHandle sock = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    return hdl

handleForPlayer :: [(Player, Handle)] -> Player -> Handle
handleForPlayer hdlsMap p = snd $ head $ filter ((== p) . fst) hdlsMap

tellWin :: Player -> (Player -> Handle) -> GameState ()
tellWin p h4p = tellAndClose h4p ((if p == R then "Red" else "Yellow") ++ " wins\n")

tellDraw :: (Player -> Handle) -> GameState ()
tellDraw h4p = tellAndClose h4p "Draw"

tellAndClose :: (Player -> Handle) -> String -> GameState ()
tellAndClose h4p msg = do
    let hdlR = h4p R
        hdlY = h4p Y
    lift $ hPutStrLn hdlR msg
    lift $ hClose hdlR
    lift $ hPutStrLn hdlY msg
    lift $ hClose hdlY
