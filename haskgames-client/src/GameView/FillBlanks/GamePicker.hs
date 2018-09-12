{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecursiveDo, FlexibleContexts, NoMonomorphismRestriction, TemplateHaskell #-}
module GameView.FillBlanks.GamePicker where
    import Reflex.Dom
    import Data.Monoid
    import Data.FileEmbed
    import Game.FillBlanks.Game
    import Data.Aeson
    import qualified Data.Text as T
    import Data.Text.Encoding as E
    import Control.Lens 
    import Data.ByteString hiding (elem, concat)
    import Control.Monad.Fix
    import qualified Data.ByteString.Lazy as BS 


    gamePickerWidget :: (Reflex t, MonadHold t m, MonadFix m, DomBuilder t m, PostBuild t m)
                     => WebSocket t -> m (Event t [BS.ByteString])
    gamePickerWidget ws = el "div" $ do
        ds <- gameList $ ws & _webSocket_recv
        joinEvents <- el "ul" $ simpleList ds showGame
        let selectEvent = switchPromptlyDyn (joinEvents <&> leftmost)
        b <- button "Refresh List"
        let broadcastEvt = leftmost [ const [(encode ListGames)] <$> b
                                    , pure . encode . JoinGame <$> selectEvent
                                    ]
        return broadcastEvt

    showGame :: (Reflex t, MonadHold t m, MonadFix m, DomBuilder t m, PostBuild t m)
             => Dynamic t GameInfo -> m (Event t Int)
    showGame g = el "div" $ do
        el "h1" $ display $ g <&> (^. gameInfoId)
        btn <- el "div" $ do
            el "h2" $ text "Using Decks:"
            el "ul" $
                simpleList (g <&> (^. gameInfoDecks)) display
            btn <- button "Join Game"
            return btn 
        return $ tagPromptlyDyn (g <&> (^. gameInfoId)) btn

    gameList :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
             => Event t ByteString
             -> m (Dynamic t [GameInfo])
    gameList e = foldDyn folder [] (encodeCoordination e)
        where
            folder (ReadInfo info) _ = info
            folder _ s = s 

    encodeCoordination :: (Reflex t)
                       => Event t ByteString
                       -> Event t CoordinationResponse
    encodeCoordination e = fmapMaybe decodeStrict e 