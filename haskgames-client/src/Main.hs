{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecursiveDo, FlexibleContexts, NoMonomorphismRestriction, TemplateHaskell #-}
    module Main where
    import Reflex.Dom
    import Data.Monoid
    import Data.FileEmbed
    import Game.FillBlanks.Game
    import Game.FillBlanks.Event
    import Game.FillBlanks.Deck
    import Data.Aeson
    import qualified Data.Text as T
    import Data.Text.Encoding as E
    import Control.Lens 
    import Data.ByteString hiding (elem, concat)
    import Control.Monad.Fix
    import Data.List (delete)
    import qualified Data.Map as Map
    import qualified Data.ByteString.Lazy as BS
    import GameView.FillBlanks.Main
    import Data.Text.Encoding (decodeUtf8)

    headWidget :: (MonadWidget t m)
               => m ()
    headWidget = do 
        el "style" $ (text . decodeUtf8) $(embedFile "static/main.css")
        elAttr "meta" ("charset" =: "UTF-8") $ return ()


    main = mainWidgetWithHead headWidget $ el "div" $ mdo
        ws <- webSocket "ws://localhost:9000" $ def &
            webSocketConfig_send .~ leftmost [listEvt]
        listEvt <- fullWorkflow ws
        return ()