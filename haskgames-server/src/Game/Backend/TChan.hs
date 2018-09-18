{-# LANGUAGE TemplateHaskell
           , DeriveGeneric
           , OverloadedStrings
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
    import Game.Backend.Common
    import Control.Monad.STM
    import Control.Concurrent.STM.TVar
    import qualified Data.ByteString.Lazy.Char8 as BSE
    import Control.Monad.Reader
    import Control.Monad.Reader.Class
    import Control.Monad.IO.Class
    import Control.Monad.State.Class
    import Control.Monad.Fix
    import Control.Applicative
    import Control.Monad.Writer.Class
    import Control.Monad.Error.Class
    import Control.Monad.Zip
    import Control.Monad.Cont.Class
    import Control.Monad.Random.Class 

    data Backend s r t
        = Backend
        { _backendBroadcast :: TChan (SendMessage s)
        , _backendRecv :: TChan (RecvMessage r)
        , _backendState :: TVar (Maybe t)
        }

    makeLenses ''Backend

    newtype ChannelBackendT s r t m a = 
        ChannelBackendT { getChannelBackendT :: ReaderT (Backend s r t) m a }
        deriving (Functor, Applicative, Monad, MonadTrans,
                  MonadIO)

    runChannelBackendT :: ChannelBackendT s r t m a
                       -> Backend s r t
                       -> m a
    runChannelBackendT = runReaderT . getChannelBackendT

    -- Look at all this boilerplate! Amazing, isn't it?
    deriving instance (Monad m) => MonadReader (Backend s r t) (ChannelBackendT s r t m)
    deriving instance (MonadState s m) => MonadState s (ChannelBackendT s' r t m)
    deriving instance (MonadError e m) => MonadError e (ChannelBackendT s r t m)
    deriving instance (MonadWriter w m) => MonadWriter w (ChannelBackendT s r t m)
    deriving instance (MonadFix m) => MonadFix (ChannelBackendT s r t m)
    deriving instance (Alternative m) => Alternative (ChannelBackendT s r t m)
    deriving instance (MonadPlus m) => MonadPlus (ChannelBackendT s r t m)
    deriving instance (MonadZip m) => MonadZip (ChannelBackendT s r t m)
    deriving instance (MonadCont m) => MonadCont (ChannelBackendT s r t m)
    deriving instance (MonadRandom m) => MonadRandom (ChannelBackendT s r t m)

    newBackend :: STM (Backend s r t)
    newBackend = Backend <$> newBroadcastTChan <*> newTChan <*> (newTVar Nothing)

    newBackendIO :: IO (Backend s r t)
    newBackendIO = Backend <$> newBroadcastTChanIO <*> newTChanIO <*> (newTVarIO Nothing)

    instance (MonadIO m, Monad m) => (MonadLog m) where
        logJSON m = liftIO $ BSE.putStrLn (encode m) 
    
    instance (Monad m, MonadIO m) => MonadBroadcaster s (ChannelBackendT s r t m) where
        broadcast msg = do
            backend <- ask
            let chan = backend ^. backendBroadcast
            liftIO $ atomically $ writeTChan chan (BroadcastMessage msg)

    instance (Monad m, MonadIO m) => MonadSender s (ChannelBackendT s r t m) where
        sendPlayer p msg = do
            backend <- ask
            let chan = backend ^. backendBroadcast
            liftIO $ atomically $ writeTChan chan (DirectedMessage p msg)

    instance (Monad m, MonadIO m) => MonadRecv r (ChannelBackendT s r t m) where
        recvEvent = do
            backend <- ask
            let chan = backend ^. backendRecv
            liftIO . putStrLn $ "Trying to read from the channel..."
            liftIO $ ((atomically $ readTChan chan) <* putStrLn "Okay we read nice")
        
    instance (Monad m, MonadIO m) => MonadGamePublic t (ChannelBackendT s r t m) where
        tellPublic s = do
            backend <- ask
            liftIO $ atomically $ 
                writeTVar (backend ^. backendState) $ Just s
        modifyPublic f = do
            backend <- ask
            liftIO $ atomically $ modifyTVar (backend ^. backendState) $ fmap f