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
    import Control.Lens
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

    logJSON :: (ToJSON a, MonadLog m) => a -> m ()
    logJSON = logMessage . toStrict . toLazyText . encodeToTextBuilder . toJSON

    instance MonadBroadcaster e Identity where
        broadcast _ = return ()
       
    instance MonadSender e Identity where
        sendPlayer  _ _ = return ()

    instance MonadLog Identity where
        logMessage _ = return ()

    class Monad m => MonadBroadcaster e m where
        broadcast :: e -> m ()
       
    class Monad m => MonadSender e m where
        sendPlayer :: PlayerId -> e -> m ()

    class Monad m => MonadRecv e m | m -> e where
        recvEvent :: m (RecvMessage e)

    class Monad m => MonadLog m where
        logMessage :: T.Text -> m ()

    type MonadGame e m 
        = ( MonadBroadcaster e m
          , MonadSender e m
          , MonadLog m)