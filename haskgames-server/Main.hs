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
    import Game.FillBlanks.Game
    import Game.FillBlanks.Deck
    import Game.Common
    import GHC.Generics
    import Control.Lens
    import Control.Concurrent.STM
    import Control.Concurrent.STM.TChan
    import Control.Concurrent.STM.TVar
    import qualified Data.Text as T
    import Control.Monad
    import Control.Exception
    import Game.Backend.Common

    import Data.Aeson

    

    type Backend' = Backend ServerEvent ClientEvent GamePublic

    type ReaderType a = ReaderT Backend' IO a

    receiveJson :: (FromJSON a) => Connection -> IO (Maybe a)
    receiveJson c = decode <$> receiveData c

    sendJson :: (ToJSON a) => Connection -> a -> IO ()
    sendJson c m = sendTextData c (encode m)

    cards = ["YEFH8","YEFH8","YEFH8","YEFH8","YEFH8","YEFH8","YEFH8","YEFH8"]

    game = do
        d <- cardCastsToDeck cards
        return $
            Game "0" 10 d d mempty AwaitingSubmissions Nothing mempty

    counter = newTVarIO (0 :: Int)

    addAndReturn s = do
        r <- readTVar s
        modifyTVar s (+1)
        return r 


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
                e <- receiveJson conn
                case e of
                    Just e' -> atomically $ writeTChan chan $ GameEvent id e'
                    Nothing -> putStrLn "Failed to read a message, that's bad"
            disconnect :: SomeException -> IO ()
            disconnect e =
                atomically $ writeTChan chan (PlayerDisconnected id)
                
    joinGame backend conn id = do
        broadcast <- atomically $ dupTChan $ backend ^. backendBroadcast
        send <- atomically $ dupTChan $ backend ^. backendRecv
        atomically $ writeTChan send $ PlayerConnected id 
        forkIO $ gameRead conn id broadcast
        gameWrite conn id send

    serverApp :: Backend' -> TVar Int -> PendingConnection -> IO ()
    serverApp backend counter pc = do
        conn <- acceptRequest pc
        forkPingThread conn 30
        id <- atomically $ addAndReturn counter
        joinGame backend conn (T.pack $ show $ id)

    runGameBackend' :: Game -> Backend' -> IO ()
    runGameBackend' game backend = runReaderT (serve game) backend 

    runGameBackend :: Game -> IO Backend'
    runGameBackend game = do
        backend <- newBackendIO
        forkIO $ runGameBackend' game backend
        return backend

    events = [ StartRound "playerId" (CallCard "Who did it? _." 1 FillIn) 
             , StartJudgement [JudgementCase mempty 1]
             , RoundWinner (JudgementCase mempty 1) "playerId"
             , UpdateScores mempty
             , GameWinner "playerId"
             , InvalidSend "That's incorrect"
             , DealCards [ResponseCard "Alex Jones" FillIn] ]
    main = do
        g <- game 
        c <- counter
        s <- runGameBackend g
        print $ map encode events 
        runServer "0.0.0.0" 9000 $ serverApp s c