{-# LANGUAGE TemplateHaskell
           , DeriveAnyClass
           , DeriveGeneric
           , OverloadedStrings
 #-}
module Main where
    import Network.WebSockets
    import Game.FillBlanks.Main

    main = do
        runServer "0.0.0.0" 9000 $ serverApp