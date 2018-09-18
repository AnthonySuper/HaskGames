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

    import Game.Common
    import Control.Lens
    import Data.Aeson
    import qualified Data.Text as T
    import qualified Data.ByteString.Lazy as BS

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
        --
    