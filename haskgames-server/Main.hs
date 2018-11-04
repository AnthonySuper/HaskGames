{-# LANGUAGE TemplateHaskell
           , DeriveAnyClass
           , DeriveGeneric
           , OverloadedStrings
 #-}
module Main where
    import Network.WebSockets
    import Game.FillBlanks.Main
    import Snap
    import Network.WebSockets.Snap
    import Snap.Util.FileServe (serveDirectory)
    import Control.Applicative

    site :: Snap ()
    site =
        route [("ws", runWebSocketsSnap serverApp)] <|>
        serveDirectory "public"
    

    main = do
        putStrLn "Starting server..."
        quickHttpServe site 