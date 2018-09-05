{-# LANGUAGE TemplateHaskell
           , DeriveAnyClass
           , DeriveGeneric
           , OverloadedStrings
 #-}
module Main where

    import Game.FillBlanks.Coordinator
    import Game.Backend.TChan
    import Control.Monad.Reader
    import Game.FillBlanks.Event
    import Network.WebSockets
    import Control.Concurrent.MVar
    import Control.Concurrent (forkIO, threadDelay)
    import qualified Data.ByteString as B
    import Game.FillBlanks.Event
    import Game.FillBlanks.Server
    import Game.FillBlanks.ServerState
    import Game.Common
    import GHC.Generics
    import Control.Lens
    import Control.Concurrent.STM
    import Control.Concurrent.STM.TChan
    import qualified Data.Text as T
    import Control.Monad
    import Control.Exception
    import Game.Backend.Common

    import Data.Aeson

    {-

    type Backend' = Backend ServerEvent ClientEvent ()

    type ReaderType a = ReaderT Backend' IO a

    receiveJson :: (FromJSON a) => Connection -> IO (Maybe a)
    receiveJson c = decode <$> receiveData c

    sendJson :: (ToJSON a) => Connection -> a -> IO ()
    sendJson c m = sendTextData c (encode m)

    cards = ["YEFH8","YEFH8","YEFH8","YEFH8","YEFH8","YEFH8","YEFH8","YEFH8"]
    config = Configuration 10 cards "A test game yo"

    data NameMessage = NameMessage { nameMessageName :: T.Text }
        deriving (Generic, FromJSON)

    getName :: Connection -> IO T.Text
    getName conn = do
        r <- receiveJson conn
        case r of
            Nothing -> putStrLn "ERR: Could not get name" >> getName conn
            Just nm -> return $ nameMessageName nm

    runBackend :: Backend' -> FillBlanksState -> IO ()
    runBackend b fbs = runReaderT (runGame fbs) b >> return ()

    createGame' :: MVar Backend' -> T.Text -> IO ()
    createGame' bs t = do
        backend <- newBackendIO
        putStrLn "LOG: Created a backend"
        putMVar bs backend
        putStrLn "LOG: Put the backend in the MVar successfully"
        game <- createGame config t
        putStrLn $ "LOG: Created game " ++ (show game)
        putStrLn "LOG: Forking game state thread"
        forkIO $ runBackend backend $ GameState mempty game
        return ()


    gameRead :: Connection -> T.Text -> TChan (SendMessage ServerEvent) -> IO ()
    gameRead c id chan = forever $ do
        putStrLn $ "BDC: Reading broadcast messages for user " ++ (show id)
        res <- atomically $ readTChan chan 
        case res of
            BroadcastMessage m -> sendJson c m
            DirectedMessage i m -> when (id == i) $ sendJson c m

    gameWrite :: Connection -> T.Text -> TChan (RecvMessage ClientEvent) -> IO ()
    gameWrite conn id chan = catch l disconnect
        where
            l = forever $ do
                putStrLn $ "RCV: Attempting to read from the connection"
                e <- receiveJson conn
                case e of
                    Just e' -> atomically $ writeTChan chan $ GameEvent id e'
                    Nothing -> putStrLn "Failed to read a message, that's bad"
            disconnect :: SomeException -> IO ()
            disconnect e = do
                putStrLn $ "ERR: Got exception " ++ (show e)
                print $ "LOG: A client named " `T.append` id `T.append` " just disconnected"
                atomically $ writeTChan chan (PlayerDisconnected id)
                
    joinGame mv conn id = do
        b <- readMVar mv
        broadcast <- atomically $ dupTChan $ b ^. backendBroadcast
        send <- atomically $ dupTChan $ b ^. backendRecv
        forkIO $ gameRead conn id broadcast
        forkIO $ gameWrite conn id send
        return ()

    serverApp :: MVar Backend' -> PendingConnection -> IO ()
    serverApp mv pc = do
        conn <- acceptRequest pc
        forkPingThread conn 30
        putStrLn "LOG: Just accepted a request"
        n <- getName conn
        putStrLn $ "LOG: Just recieved the name`" ++ (show n) ++ "`"
        v <- isEmptyMVar mv
        putStrLn $ "LOG: Is the MVar empty? " ++ (show v)
        if v then do
            createGame' mv n
            joinGame mv conn n
        else do
            joinGame mv conn n
        s <- newEmptyMVar :: IO (MVar ())
        readMVar s

    main = do
        s <- newEmptyMVar
        print "Starting run of server"
        runServer "0.0.0.0" 9000 $ serverApp s

-}

    main = do 
        putStrLn "Whatever, man!"