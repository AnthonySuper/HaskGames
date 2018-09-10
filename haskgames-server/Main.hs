{-# LANGUAGE TemplateHaskell
           , DeriveAnyClass
           , DeriveGeneric
           , OverloadedStrings
 #-}
module Main where
    import Network.WebSockets
    import Game.FillBlanks.Coordinator
    import Game.Backend.TChan
    import Control.Monad.Reader
    import Game.FillBlanks.Event
    import Control.Concurrent.MVar
    import Control.Concurrent (forkIO, threadDelay)
    import qualified Data.ByteString as B
    import Game.FillBlanks.Event
    import Game.FillBlanks.Server
    import Game.FillBlanks.ServerState
    import Game.FillBlanks.Game
    import Game.FillBlanks.Deck
    import Game.Common
    import GHC.Generics
    import Control.Lens
    import Control.Concurrent.STM
    import Control.Concurrent.STM.TChan
    import Control.Concurrent.STM.TVar
    import qualified Data.Text as T
    import Control.Monad
    
    import Game.Backend.Common
    import Data.Aeson

    main = do
        print $ (encode $ CreateGame ["xvhv"])
        runServer "0.0.0.0" 9000 $ serverApp