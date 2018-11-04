{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , RecursiveDo
           , FlexibleContexts
           , NoMonomorphismRestriction
           , TemplateHaskell
           , TypeFamilies #-}
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
    import GHCJS.DOM.Window (getLocation)
    import GHCJS.DOM (currentWindowUnchecked)
    import GHCJS.DOM.Location (getHost)
    import GHCJS.DOM.Types (MonadJSM)
    import System.IO.Unsafe (unsafePerformIO)
    import Data.Functor (($>))


    isLocal :: T.Text -> Bool 
    isLocal s = s `elem` localHosts
        where
            localHosts = ["localhost", "0.0.0.0", ""]


    getWebsocketLocation :: (MonadJSM m) => m T.Text
    getWebsocketLocation = do
        h <- host
        return $ mconcat ["ws://", h, "/ws"]
        where
            host = currentWindowUnchecked >>= getLocation >>= getHost
    
    

    headWidget :: (MonadWidget t m)
               => m ()
    headWidget = do 
        styleSheet "http://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.2/css/bulma.css"
        el "style" $ (text . decodeUtf8) $(embedFile "static/main.css")
        elAttr "meta" ("charset" =: "UTF-8") $ return ()
        elAttr "meta" 
            ("name" =: "viewport" 
            <> "content" =: "width=device-width, initial-scale=1")
            $ blank 
        where
            styleSheet url = 
                elAttr "link" ("rel" =: "stylesheet" <> "href" =: url) $ blank 

    wsSection loc = mdo 
        ws <- webSocket loc $ def &
            webSocketConfig_send .~ (traceEvent "Got WS Event" listEvt)
        listEvt <- fullWorkflow ws
        return ()

    main = mainWidgetWithHead headWidget $ el "div" $ mdo
        pb <- getPostBuild 
        loc <- performEvent (pb $> getWebsocketLocation)
        widgetHold (return ()) (wsSection <$> loc)
        return ()