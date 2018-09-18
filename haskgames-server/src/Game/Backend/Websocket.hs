{-# LANGUAGE TemplateHaskell
           , DeriveGeneric
           , OverloadedStrings
           , DeriveAnyClass
           , MultiParamTypeClasses
           , TypeFamilies
           , FlexibleInstances
           , ConstraintKinds
           , FunctionalDependencies #-}
module Game.Backend.Websocket where

    import Game.Backend.Common
    import qualified Network.WebSockets as WS

    instance PlayerMessenger WS.Connection where
        -- Send a message, possibly blocking to do so
        sendMessage = WS.sendTextData
        -- Receive a message, possibly blocking
        recvMessage = WS.receiveData