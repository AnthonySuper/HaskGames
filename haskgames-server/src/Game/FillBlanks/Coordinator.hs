{-# LANGUAGE TemplateHaskell, DeriveAnyClass, DeriveGeneric, LambdaCase #-}
module Game.FillBlanks.Coordinator where

    import GHC.Generics
    import qualified Data.Text as T
    import Control.Lens
    import Data.Aeson
    import qualified Data.Map as Map
    import Control.Concurrent.STM
    import Control.Concurrent.STM.TVar
    import Control.Concurrent.STM.TChan
    import Control.Concurrent.Async (mapConcurrently)
    import Game.FillBlanks.CardCast
    import Game.FillBlanks.Deck
    import Game.Common
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

    type Backend' = Backend ServerEvent ClientEvent GameInfo

    data CoordinationMessage
        = JoinGame Int
        | CreateGame [String]
        | ListGames
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    data CoordinationResponse
        = ReadInfo [GameInfo]
        | JoinedGame GameInfo
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    receiveJson :: (FromJSON a) => Connection -> IO (Maybe a)
    receiveJson c = decode <$> receiveData c

    sendJson :: (ToJSON a) => Connection -> a -> IO ()
    sendJson c m = sendTextData c (encode m)

    data Coordination
        = Coordination
        { _coordinationGames :: [Backend']
        , _coordinationCounter :: Int
        }
    makeLenses ''Coordination

    cardCoordinator :: TVar Coordination
    cardCoordinator = unsafePerformIO (newTVarIO $ Coordination [] 0)

    getGames :: IO [Backend']
    getGames 
        = readTVarIO cardCoordinator
        <&> (^. coordinationGames)

    runGameBackend :: Game -> Backend' -> IO ()
    runGameBackend game backend = runReaderT (serve game) backend

    createGame :: [String] -> IO Backend'
    createGame decks = do 
        cards <- cardCastsToDeck decks
        backend <- atomically $ do
            coordinator <- readTVar cardCoordinator
            backend <- newBackend
            let ns = GameInfo (map T.pack decks) mempty (coordinator ^. coordinationCounter) 
            writeTVar (backend ^. backendState) $ Just ns
            modifyTVar cardCoordinator (coordinationCounter %~ (+1))
            return backend
        let game = Game 10 cards cards mempty
        forkIO $ runGameBackend game backend
        return backend

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

    newPlayer :: Connection -> T.Text -> IO ()
    newPlayer c id = receiveJson c >>= \case
        Just x -> coordinate c id x
        Nothing -> newPlayer c id

    coordinate :: Connection -> T.Text -> CoordinationMessage -> IO ()
    coordinate c id f = case f of
        CreateGame decks -> do
            b <- createGame decks
            joinGame b c id
        ListGames -> do
            infos <- getInfo
            sendJson c infos
            newPlayer c id
        JoinGame i -> do
            backend <- getBackend i
            case backend of
                Nothing -> newPlayer c id
                Just backend' -> joinGame backend' c id
        
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
        newPlayer conn (T.pack $ show id)

    getInfo :: IO [GameInfo]
    getInfo 
        = (^. coordinationGames) <$> readTVarIO cardCoordinator
        >>= mapM (readTVarIO . (^. backendState))
        <&> catMaybes

    getBackend :: Int -> IO (Maybe Backend')
    getBackend i = do
        coordinator <- readTVarIO cardCoordinator
        withId <- filterM (hasId i) (coordinator ^. coordinationGames)
        return $ listToMaybe withId 

    hasId :: Int -> Backend' -> IO Bool
    hasId i x = do 
        r <- readTVarIO (x ^. backendState)
        let id' = r <&> (^. gameInfoId)
        return $ id' == Just i
            

    cardCastsToDeck :: [String] -> IO CardDeck
    cardCastsToDeck s = do
        decks <- mapConcurrently getCardDeck s
        let real = catMaybes decks
        return $ fold real