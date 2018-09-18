{-# LANGUAGE TemplateHaskell, DeriveAnyClass, DeriveGeneric, LambdaCase #-}

module Game.FillBlanks.Main where

    import GHC.Generics
    import qualified Data.Text as T
    import Control.Lens
    import Data.Aeson
    import Game.Backend.Common
    import qualified Data.Map as Map
    import Control.Concurrent.STM
    import Control.Concurrent.STM.TVar
    import Control.Concurrent.STM.TChan
    import Control.Concurrent.Async (mapConcurrently)
    import Game.FillBlanks.CardCast
    import Game.FillBlanks.Deck
    import Game.Common
    import Game.Basic
    import Game.FillBlanks.ServerState
    import Data.Foldable
    import Data.Maybe
    import Network.WebSockets
    import Game.Backend.TChan
    import Game.FillBlanks.Game
    import Game.FillBlanks.Event
    import Game.FillBlanks.Server
    import Control.Concurrent
    import Control.Exception
    import System.IO.Unsafe (unsafePerformIO)
    import Control.Monad.Reader
    import Game.FillBlanks.Coordinator
    import Control.Monad.State.Strict

    data PlayerThreadType = WriterThread | ReaderThread
        deriving (Show)

    data LogMessage
        = GameThreadStarted ThreadId
        | GameThreadEnded ThreadId
        | PlayerThreadStarted PlayerId PlayerThreadType ThreadId
        | PlayerThreadEnded PlayerId PlayerThreadType ThreadId
        deriving (Show)

    printWithTID a = print =<< a <$> myThreadId 

    receiveJson :: (FromJSON a) => Connection -> IO (Maybe a)
    receiveJson c = decode <$> receiveData c

    sendJson :: (ToJSON a) => Connection -> a -> IO ()
    sendJson c m = sendTextData c (encode m)

    runGameBackend :: Game -> Backend' -> IO ()
    runGameBackend game backend = bracket_ printStart printEnd $ run
        where
            run =
                evalStateT (runChannelBackendT serve backend) game
            printStart = printWithTID GameThreadStarted
            printEnd = printWithTID GameThreadEnded

    gameRead :: Connection -> T.Text -> TChan (SendMessage ServerEvent) -> IO ()
    gameRead c id chan = msg >> catch l handle 
        where
            handle :: SomeException -> IO ()
            handle e = do 
                printWithTID $ PlayerThreadEnded id ReaderThread
            l = forever $ do
                res <- atomically $ readTChan chan
                case res of
                    BroadcastMessage m -> sendJson c m
                    DirectedMessage i m -> when (id == i) $ sendJson c m
            msg = printWithTID $ PlayerThreadStarted id ReaderThread

    disconnectMsg pid e = print $ concat msg 
        where
            msg = [show pid, " disconnected due to ", show e]
            
    gameWrite :: Connection -> T.Text -> TChan (RecvMessage ClientEvent) -> IO ()
    gameWrite conn id chan = catch (msg >> l) disconnect
        where
            msg = printWithTID $ PlayerThreadStarted id WriterThread
            l = forever $ do
                e <- receiveJson conn
                case e of
                    Just e' -> atomically $ writeTChan chan $ GameEvent id e'
                    Nothing -> putStrLn "Failed to read a message, that's bad"
            disconnect :: SomeException -> IO ()
            disconnect e = do
                printWithTID $ PlayerThreadEnded id WriterThread   
                atomically $ writeTChan chan (PlayerDisconnected id)
                
    joinGame backend conn id = do
        broadcast <- atomically $ dupTChan $ backend ^. backendBroadcast
        let send = backend ^. backendRecv 
        forkIO $ gameRead conn id broadcast
        atomically $ writeTChan send $ PlayerConnected id
        gameWrite conn id send

    newPlayer :: Connection -> T.Text -> IO ()
    newPlayer c id = receiveJson c >>= \case
        Just x -> coordinate c id x
        Nothing -> do
            print ("Player ", id, " sent an invalid msg")
            newPlayer c id

    coordinate :: Connection -> T.Text -> CoordinationMessage -> IO ()
    coordinate c id f = case f of
        CreateGame decks -> do
            print "Creating a game..."
            (g, b) <- createGame decks
            print "Game created! Running backend"
            forkIO $ runGameBackend g b
            sendJson c $ JoinedGame
            joinGame b c id
        ListGames -> do
            print "Listing games..."
            infos <- getInfo
            let toSend = ReadInfo infos
            sendJson c toSend
            newPlayer c id
        JoinGame i -> do
            print "Joining a game..."
            backend <- getBackend i
            case backend of
                Nothing -> newPlayer c id
                Just backend' -> do
                    print "Found a working backend, joining..."
                    sendJson c $ JoinedGame
                    joinGame backend' c id
        
    backendCounter :: TVar Integer
    backendCounter = unsafePerformIO $ newTVarIO 0

    addAndReturn s = do
        r <- readTVar s
        modifyTVar s (+1)
        return r 

    serverApp :: PendingConnection -> IO ()
    serverApp pc = do
        conn <- acceptRequest pc 
        forkPingThread conn 30
        id <- atomically $ addAndReturn backendCounter
        print "Found a new player, asking for commands..."
        newPlayer conn (T.pack $ show id)
