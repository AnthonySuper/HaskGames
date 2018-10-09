{-# LANGUAGE TemplateHaskell
           , DeriveAnyClass
           , DeriveGeneric
           , OverloadedStrings
 #-}
module Main where
    import Network.WebSockets
    import Game.FillBlanks.Main

    main = do
        putStrLn "Starting server on port 9000..."
        runServer "0.0.0.0" 9000 $ serverApp