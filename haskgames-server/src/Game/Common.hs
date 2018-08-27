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
        | Tick 
        | GameEvent PlayerId re
        deriving (Show, Eq, Read, Generic, FromJSON)

    instance MonadBroadcaster e Identity where
        broadcast _ = return ()
       
    instance MonadSender e Identity where
        sendPlayer  _ _ = return ()

    class Monad m => MonadBroadcaster e m where
        broadcast :: e -> m ()
       
    class Monad m => MonadSender e m where
        sendPlayer :: PlayerId -> e -> m ()

    class Monad m => MonadRecv e m | m -> e where
        recvEvent :: m (RecvMessage e)

    type MonadGame e m = (MonadBroadcaster e m, MonadSender e m)