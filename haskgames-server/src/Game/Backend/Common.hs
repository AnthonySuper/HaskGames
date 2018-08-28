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

    runGame :: ( MonadGame se m
               , MonadRecv ce m
               , Game ps cs se ce)
            => GameState ps cs
            -> m ()
    runGame s = do 
        e <- recvEvent
        case e of
            PlayerConnected id -> handleConnect s id
            PlayerDisconnected id -> handleDisconnect s id
            GameEvent id re -> handleGameEvent s id re

    handleGameEvent s id r = do
        s' <- serve s id r
        runGame s'
    
    handleConnect s id = do
        newS <- playerWillConnect s id
        ps <- playerConnectState s id
        let newS' = newS & playerState . at id .~ Just ps
        playerDidConnect newS' id >>= runGame

    handleDisconnect s id = do
        let withoutPlayer = s & playerState . at id .~ Nothing
        newS <- playerDisconnected s withoutPlayer id
        runGame newS

    -- | The @Game@ class provides an interface for types which one can play games on.
    -- It takes several type parameters, representing:
    --
    --   [@ps@] The state of each individual player in the game
    --
    --   [@cs@] The common state shared by players of the game
    --
    --   [@se@] The server-sent events broadcast to the players of the game
    --
    --   [@ce@] The client-sent events used to update the game
    --
    -- Instances of this class allow games to be played while abstracted over a variety
    -- of different "backends," allowing for a high degree of generalization.
    class Game ps cs se ce | ps cs -> se, ps cs -> ce where
        -- | Given the current state, the player who sent us an event, and the
        -- event itself, determine what the new state is.
        serve :: (MonadGame se m)
                => GameState ps cs
                -> PlayerId
                -> ce
                -> m (GameState ps cs)
        -- | Given the current state, and the ID of the player connecting,
        -- determine what the player-specific state for that new player should be
        playerConnectState :: (MonadGame se m)
                            => GameState ps cs
                            -> PlayerId
                            -> m ps
        -- | Given the information that a player with the given ID is connecting,
        -- update the state of the overall game to accomodate the new player.
        -- By default, does nothing.
        playerWillConnect :: (MonadGame se m)
                          => GameState ps cs
                          -> PlayerId
                          -> m (GameState ps cs)
        -- | Given the information that a player with the given id has connected,
        -- perform any broadcasts in response to this connection, including
        -- any player-specific broadcasts to get the new player up to date on the 
        -- state of the game. By default, does nothing.
        playerDidConnect :: (MonadGame se m)
                         => GameState ps cs
                         -> PlayerId
                         -> m (GameState ps cs)
        -- | Given the state before a player disconnected, the state *after* a player
        -- has disconnected (IE, their entry has been removed from the playerState map),
        -- and the player who disconnected, create the new state for the given player
        playerDisconnected :: (MonadGame se m)
                           => (GameState ps cs)
                           -> (GameState ps cs)
                           -> PlayerId
                           -> m (GameState ps cs)
