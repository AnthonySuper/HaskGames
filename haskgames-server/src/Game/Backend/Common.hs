{-# LANGUAGE TemplateHaskell
           , DeriveGeneric
           , OverloadedStrings
           , DeriveAnyClass
           , MultiParamTypeClasses
           , TypeFamilies
           , FlexibleInstances
           , ConstraintKinds
           , FunctionalDependencies #-}
module Game.Backend.Common where

    import Control.Exception
    import Game.Common
    import Game.Basic
    import Control.Lens
    import Data.Aeson
    import qualified Data.Text as T
    import qualified Data.ByteString.Lazy as BS
    import Control.Monad (when, forever)
    import Control.Concurrent (forkIO)

    data SendMessage a 
        = BroadcastMessage a
        | DirectedMessage T.Text a

    sendJSONMessage :: (ToJSON a, PlayerMessenger m)
                    => m -> a -> IO ()
    sendJSONMessage m a = sendMessage m (encode a)

    recvJSONMessage :: (FromJSON a, PlayerMessenger m)
                    => m -> IO (Maybe a)
    recvJSONMessage m = decode <$> recvMessage m

    -- | A typeclass representing things that are "like sockets" in that they can
    -- | send and receive strings of bytes. 
    class PlayerMessenger m where
        -- Send a message, possibly blocking to do so
        sendMessage :: m -> BS.ByteString -> IO ()
        -- Receive a message, possibly blocking
        recvMessage :: m -> IO BS.ByteString  
        --

    class GameBackend b s r | b -> s, b -> r where
        getBackendBroadcast :: b -> IO (SendMessage s)
        putBackendMessage :: b -> RecvMessage r -> IO ()
        -- Prepare a backend for use, if needed
        prepareBackend :: b -> IO b 
        prepareBackend = return 
    
    joinGame :: (GameBackend b s r, PlayerMessenger m, ToJSON s, FromJSON r)
             => b -> m -> PlayerId -> IO () 
    joinGame backend messenger id = do
        backend' <- prepareBackend backend
        forkIO $ readLoop backend' messenger id 
        putBackendMessage backend' $ PlayerConnected id
        writeLoop backend' messenger id

    writeLoop backend messenger pid = forever $ do
        e <- recvJSONMessage messenger
        case e of
            Just e' -> putBackendMessage backend $ GameEvent pid e'
            Nothing -> return ()
    
    readLoop backend messenger pid 
        = flip finally disconnect $ forever $ readLoop'
        where
            disconnect = putBackendMessage backend $ PlayerDisconnected pid 
            sendMessage = sendJSONMessage messenger
            readLoop' = do 
                res <- getBackendBroadcast backend 
                case res of 
                    BroadcastMessage m -> sendMessage m
                    DirectedMessage i m -> when (i == pid) $ sendMessage m

