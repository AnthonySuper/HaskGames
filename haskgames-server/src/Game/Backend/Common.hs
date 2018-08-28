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

    class Game ps cs se ce | ps cs -> se, ps cs -> ce where
        serve :: (MonadGame se m)
                => GameState ps cs
                -> PlayerId
                -> ce
                -> m (GameState ps cs)
        playerConnectState :: (MonadGame se m)
                            => GameState ps cs
                            -> PlayerId
                            -> m ps
        playerWillConnect :: (MonadGame se m)
                          => GameState ps cs
                          -> PlayerId
                          -> m (GameState ps cs)
        playerDidConnect :: (MonadGame se m)
                         => GameState ps cs
                         -> PlayerId
                         -> m (GameState ps cs)
        playerDisconnected :: (MonadGame se m)
                           => (GameState ps cs)
                           -> (GameState ps cs)
                           -> PlayerId
                           -> m (GameState ps cs)
    