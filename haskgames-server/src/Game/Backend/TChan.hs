{-# LANGUAGE TemplateHaskell
           , DeriveGeneric
           , OverloadedStrings
           , DeriveAnyClass
           , GeneralizedNewtypeDeriving
           , StandaloneDeriving
           , FlexibleInstances
           , MultiParamTypeClasses
           , FlexibleContexts
           , UndecidableInstances #-}
module Game.Backend.TChan where
    
    import GHC.Generics
    import qualified Data.Text as T
    import qualified Data.Map as M
    import Control.Lens
    import Data.Aeson
    import Control.Concurrent.STM.TChan
    import Game.Common
    import Control.Monad.Reader
    import Control.Monad.IO.Class
    import Control.Monad.STM

    data SendMessage a 
        = BroadcastMessage a
        | DirectedMessage T.Text a
    
    deriving instance (Show a) => Show (SendMessage a)
    deriving instance (Read a) => Read (SendMessage a)
    deriving instance (Eq a) => Eq (SendMessage a)
    deriving instance (Ord a) => Ord (SendMessage a)
    deriving instance (Generic a) => Generic (SendMessage a)
    deriving instance (Generic a, ToJSON a) => ToJSON (SendMessage a)

    data Backend s r
        = Backend
        { _backendBroadcast :: TChan (SendMessage s)
        , _backendRecv :: TChan (RecvMessage r)
        }

    newBackend :: STM (Backend s r)
    newBackend = Backend <$> newBroadcastTChan <*> newTChan

    newBackendIO :: IO (Backend s r)
    newBackendIO = Backend <$> newBroadcastTChanIO <*> newTChanIO

    makeLenses ''Backend

    instance (MonadReader (Backend s r) m, MonadIO m) => MonadBroadcaster s m where
        broadcast msg = do
            backend <- ask
            let chan = backend ^. backendBroadcast
            liftIO $ atomically $ writeTChan chan (BroadcastMessage msg)

    instance (MonadReader (Backend s r) m, MonadIO m) => MonadSender s m where
        sendPlayer p msg = do
            backend <- ask
            let chan = backend ^. backendBroadcast
            liftIO $ atomically $ writeTChan chan (DirectedMessage p msg)

    instance (MonadReader (Backend s r) m, MonadIO m) => MonadRecv r m where
        recvEvent = do
            backend <- ask
            let chan = backend ^. backendRecv
            liftIO $ atomically $ readTChan chan