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
    import Control.Monad.Random.Class
    import Game.Basic
    import Data.Aeson
    import Data.Text.Encoding (decodeUtf8)
    import Data.Aeson.Text (encodeToTextBuilder)
    import Data.Text.Lazy.Builder (toLazyText)
    import Data.Text.Lazy (toStrict)
    import Control.Monad.Random

    -- | The various types of messages a game must respond to.
    -- These messages are common in all games, as all games must react
    -- to the changing state of various players.
    -- Games must also react to their own player messages from players,
    -- hence this type is paramaterized over a "game event" that is
    -- specific to each type of game 
    data RecvMessage re
        -- | A message indicating that a player has connected.
        -- Games should use this to give the player some kind of
        -- player-specific state, if applicable
        = PlayerConnected PlayerId
        -- | A message indicating that a player has disconnected.
        -- Games should use this to perform some kind of cleanup.
        | PlayerDisconnected PlayerId
        -- | A message sent by a given player.
        -- Games should use this to implement gameplay
        | GameEvent PlayerId re
        deriving (Show, Eq, Read, Generic, FromJSON, ToJSON)

    -- | The MonadBroadcaster class represents the class of monads
    -- which allow events to be broadcast to all monads.
    -- This is done to allow greater polymorphism, so a game is not
    -- tied down by the implementation of communication.
    class Monad m => MonadBroadcaster e m where
        -- | Send a message to all the players in a given game
        broadcast :: e -> m ()
        --
        
    -- | The MonadSender class represents the class of monads
    -- which allow events to be sent to a specific player.
    -- This is done to allow for greater polymorphism, and to
    -- decouple the logic of communication from its implementation.
    class Monad m => MonadSender e m where
        -- | Send a message to a specific player in a game
        sendPlayer :: PlayerId -> e -> m ()
        --

    -- | The MonadRecv class represents the class of monads which
    -- allow for the recieving of game messages. This is done
    -- to allow for greater polymorphism.
    class Monad m => MonadRecv e m | m -> e where
        -- | Read an event from the ongoing stream of possible events
        recvEvent :: m (RecvMessage e)
        --

    -- | The MonadGamePublic class represents the class of monads
    -- which allow a game to update public information about itself.
    -- This information might be used to show new players information
    -- about the game.
    class Monad m => MonadGamePublic s m | m -> s where
        -- | Fully replace the public information about a game
        tellPublic :: s -> m ()
        -- | Update the public information about a game
        modifyPublic :: (s -> s) -> m ()
        --

    -- | The MonadLog class represents the class of monads with the
    -- ability to log JSON messages.
    class Monad m => MonadLog m where
        -- | Log a JSON message, possibly to some central storage
        logJSON :: (ToJSON a) => a -> m ()
        --

    -- | Helper function to log a JSON message while returning
    -- the given value
    logId :: (MonadLog m, ToJSON a)
          => a -> m a
    logId a = logJSON a >> return a 

    -- | Helper function to log any member of the Show typeclass
    -- to a JSON-logging-enabled backend.
    logShow :: (Show a, MonadLog m) => a -> m ()
    logShow = logMessage . T.pack . show

    -- | Helper function to log any textual message to
    -- a JSON-logging-enabled backend.
    logMessage :: (MonadLog m) => T.Text -> m ()
    logMessage m = logJSON json
        where
            json = object ["message" .= toJSON m]

    -- | Send a player a message, only if that event actually exists.
    -- Will log on failure. 
    sendPlayerMaybe :: (MonadSender e m, MonadLog m)
                    => PlayerId
                    -> Maybe e
                    -> m ()
    sendPlayerMaybe id v = case v of
        Just v' -> sendPlayer id v' 
        Nothing -> logShow ("SendPlayerMaybe called with Nothing", id) 

    instance MonadBroadcaster e Identity where
        broadcast _ = return ()
       
    instance MonadSender e Identity where
        sendPlayer  _ _ = return ()

    instance MonadLog Identity where
        logJSON _ = return ()
    
    -- | Constraint for monads that allow full gameplay, where
    --
    --     [@s@] is the type of server-sent events
    --
    --     [@c@] is the type of client-sent events
    --
    --     [@t@] is the type of public information
    --
    --     [@m@] is the constrained monad
    --
    type MonadGame s c t m
        = ( MonadBroadcaster s m 
          , MonadSender s m 
          , MonadLog m 
          , MonadRecv c m 
          , MonadGamePublic t m
          , MonadRandom m
          )
    
    type MonadGameOutput e m 
        = ( MonadBroadcaster e m
          , MonadSender e m
          , MonadLog m)