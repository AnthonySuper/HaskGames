{-# LANGUAGE TemplateHaskell
           , DeriveGeneric
           , OverloadedStrings
           , DeriveAnyClass
           , MultiParamTypeClasses
           , TypeFamilies
           , FlexibleInstances
           , ConstraintKinds
           , FunctionalDependencies #-}
module Game.Common where
    import GHC.Generics
    import qualified Data.Text as T
    import qualified Data.Map as M
    import Control.Lens hiding ((.=))
    import Data.Aeson
    import Data.Text.Encoding (decodeUtf8)
    import Data.Aeson.Text (encodeToTextBuilder)
    import Data.Text.Lazy.Builder (toLazyText)
    import Data.Text.Lazy (toStrict)


    type PlayerId = T.Text

    data GameState playerState commonState
        = GameState
        { _playerState :: M.Map PlayerId playerState
        , _commonState :: commonState
        }
        deriving (Show, Eq, Read, Generic)
    
    makeLenses ''GameState

    hasPlayer :: PlayerId -> GameState a b -> Bool
    hasPlayer i g = M.member i (g ^. playerState)

    numPlayers :: GameState a b -> Int
    numPlayers g = M.size (g ^. playerState)

    data RecvMessage re
        = PlayerConnected PlayerId
        | PlayerDisconnected PlayerId
        | GameEvent PlayerId re
        deriving (Show, Eq, Read, Generic, FromJSON)

    logShow :: (Show a, MonadLog m) => a -> m ()
    logShow = logMessage . T.pack . show

    logMessage :: (MonadLog m) => T.Text -> m ()
    logMessage m = logJSON json
        where
            json = object ["message" .= toJSON m]

    instance MonadBroadcaster e Identity where
        broadcast _ = return ()
       
    instance MonadSender e Identity where
        sendPlayer  _ _ = return ()

    instance MonadLog Identity where
        logJSON _ = return ()

    class Monad m => MonadBroadcaster e m where
        broadcast :: e -> m ()
       
    class Monad m => MonadSender e m where
        sendPlayer :: PlayerId -> e -> m ()

    class Monad m => MonadRecv e m | m -> e where
        recvEvent :: m (RecvMessage e)

    class Monad m => MonadStateTell s m | m -> s where
        tellState :: s -> m ()

    class Monad m => MonadLog m where
        logJSON :: (ToJSON a) => a -> m ()

    type MonadGame' s c m
        = ( MonadBroadcaster s m 
          , MonadSender s m 
          , MonadLog m 
          , MonadRecv c m )
    
    type MonadGameOutput e m 
        = ( MonadBroadcaster e m
          , MonadSender e m
          , MonadLog m)