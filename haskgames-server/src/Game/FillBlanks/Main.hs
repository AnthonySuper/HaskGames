{-# LANGUAGE TemplateHaskell, DeriveAnyClass, DeriveGeneric, LambdaCase #-}

module Game.FillBlanks.Main where

    import GHC.Generics
    import qualified Data.Text as T
    import Control.Lens
    import Data.Aeson
    import Game.Backend.Common
    import qualified Data.Map as Map
    import qualified Data.Set as Set
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
    import Game.Backend.Websocket

    data PlayerThreadType = WriterThread | ReaderThread
        deriving (Show)

    data LogMessage
        = GameThreadStarted ThreadId
        | GameThreadEnded ThreadId
        deriving (Show)

    printWithTID a = print =<< a <$> myThreadId 

    runGameBackend :: Game -> Backend' -> IO ()
    runGameBackend game backend = bracket_ printStart printEnd $ run
        where
            run =
                evalStateT (runChannelBackendT serve backend) game
            printStart = printWithTID GameThreadStarted
            printEnd = printWithTID GameThreadEnded

    newPlayer :: Connection -> T.Text -> IO ()
    newPlayer c id = recvJSONMessage c >>= \case
        Just x -> coordinate c id x
        Nothing -> do
            print ("Player ", id, " sent an invalid msg")
            newPlayer c id

    coordinate :: Connection -> T.Text -> CoordinationMessage -> IO ()
    coordinate c id f = case f of
        CreateGame creation -> do
            print ("Creating a game...", creation)
            (g, b) <- createGame creation
            print "Game created! Running backend"
            forkIO $ runGameBackend g b
            sendJSONMessage c $ JoinedGame
            joinGame b c id
        ListGames -> do
            print "Listing games..."
            infos <- getInfo
            let toSend = ReadInfo infos
            sendJSONMessage c toSend
            newPlayer c id
        JoinGame i -> do
            print "Joining a game..."
            backend <- getBackend i
            case backend of
                Nothing -> newPlayer c id
                Just backend' -> do
                    print "Found a working backend, joining..."
                    sendJSONMessage c $ JoinedGame
                    joinGame backend' c id


    nameSet :: TVar (Set.Set T.Text)
    nameSet = unsafePerformIO $ newTVarIO Set.empty

    insertName :: T.Text -> IO Bool
    insertName n = atomically $ do
        r <- readTVar nameSet
        if Set.notMember n r then do 
           modifyTVar nameSet (Set.insert n)
           return True
        else
            return False
    
    validateName conn name = do
        res <- insertName name
        if res then do
            print "Name accepted"
            sendJSONMessage conn NameAccepted
            return name 
        else do
            print "Name taken :("
            sendJSONMessage conn NameTaken
            getName conn 

    getName conn = do
        c <- recvJSONMessage conn
        case c of
            Just c' -> do
                print ("Validating name", getMsgName c') 
                validateName conn (getMsgName c')
            Nothing -> getName conn

    freeName n = 
        atomically $ modifyTVar nameSet (Set.delete n)

    serverApp :: PendingConnection -> IO ()
    serverApp pc = do
        conn <- acceptRequest pc 
        forkPingThread conn 30
        id <- getName conn
        print "Found a new player, asking for commands..."
        flip finally (freeName id) $ newPlayer conn id
