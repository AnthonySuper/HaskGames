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
    --   either send or recv messages 
    class PlayerMessenger m where
        sendMessage :: m -> BS.ByteString -> IO ()
        recvMessage :: m -> IO BS.ByteString