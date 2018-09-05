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
    import qualified Data.Text.IO as TIO
    import qualified Data.Map as M
    import Control.Lens
    import Data.Aeson
    import Control.Concurrent.STM.TChan
    import Game.Common
    import Control.Monad.Reader.Class
    import Control.Monad.IO.Class
    import Control.Monad.STM
    import Control.Concurrent.STM.TVar
    import qualified Data.ByteString.Lazy.Char8 as BSE

    data SendMessage a 
        = BroadcastMessage a
        | DirectedMessage T.Text a
    
    deriving instance (Show a) => Show (SendMessage a)
    deriving instance (Read a) => Read (SendMessage a)
    deriving instance (Eq a) => Eq (SendMessage a)
    deriving instance (Ord a) => Ord (SendMessage a)
    deriving instance (Generic a) => Generic (SendMessage a)
    deriving instance (Generic a, ToJSON a) => ToJSON (SendMessage a)

    data Backend s r t
        = Backend
        { _backendBroadcast :: TChan (SendMessage s)
        , _backendRecv :: TChan (RecvMessage r)
        , _backendState :: TVar (Maybe t)
        }

    newBackend :: STM (Backend s r t)
    newBackend = Backend <$> newBroadcastTChan <*> newTChan <*> (newTVar Nothing)

    newBackendIO :: IO (Backend s r t)
    newBackendIO = Backend <$> newBroadcastTChanIO <*> newTChanIO <*> (newTVarIO Nothing)

    makeLenses ''Backend

    instance (MonadIO m, Monad m) => (MonadLog m) where
        logJSON m = liftIO $ BSE.putStrLn (encode m) 
    
    instance (MonadReader (Backend s r t) m, MonadIO m, Show s) => MonadBroadcaster s m where
        broadcast msg = do
            backend <- ask
            let chan = backend ^. backendBroadcast
            liftIO $ print ("Trying to broadcast a message", msg)
            liftIO $ atomically $ writeTChan chan (BroadcastMessage msg)

    instance (MonadReader (Backend s r t) m, MonadIO m, Show s) => MonadSender s m where
        sendPlayer p msg = do
            backend <- ask
            let chan = backend ^. backendBroadcast
            liftIO $ atomically $ writeTChan chan (DirectedMessage p msg)

    instance (MonadReader (Backend s r t) m, MonadIO m) => MonadRecv r m where
        recvEvent = do
            backend <- ask
            let chan = backend ^. backendRecv
            liftIO $ atomically $ readTChan chan
        
    instance (MonadReader (Backend s r t) m, MonadIO m) => MonadGamePublic t m where
        tellPublic s = do
            backend <- ask
            liftIO $ atomically $ 
                writeTVar (backend ^. backendState) $ Just s
        modifyPublic f = do
            backend <- ask
            liftIO $ atomically $ modifyTVar (backend ^. backendState) $ fmap f
