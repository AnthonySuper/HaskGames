{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , RecursiveDo
           , FlexibleContexts
           , NoMonomorphismRestriction
           , TemplateHaskell
           , GADTs
           , AllowAmbiguousTypes #-}
module GameView.FillBlanks.GamePicker where
    import Reflex.Dom
    import Reflex.Helpers
    import Data.Monoid
    import Game.FillBlanks.Game
    import Data.Aeson
    import qualified Data.Text as T
    import Control.Lens 
    import Data.ByteString hiding (elem, concat)
    import Control.Monad.Fix
    import qualified Data.ByteString.Lazy as BS
    import GameView.FillBlanks.GameCreator

    gamePickerWidget :: (MonadWidget t m)
                     => WebSocket t -> m (Event t [BS.ByteString])
    gamePickerWidget ws = el "div" $ do
        ds <- gameList $ ws & _webSocket_recv
        joinEvents <- elClass "ul" "games-list" $ simpleList ds showGame
        let selectEvent = switchPromptlyDyn (joinEvents <&> leftmost)
        b <- button "Refresh List"
        createGame <- gameCreator 
        let broadcastEvt = leftmost [ const [(encode ListGames)] <$> b
                                    , pure . encode . JoinGame <$> selectEvent
                                    , createGame 
                                    ]
        return broadcastEvt

    showGame :: (MonadWidget t m)
             => Dynamic t GameInfo -> m (Event t T.Text)
    showGame g = el "div" $ do
        el "h1" $ display $ view gameInfoName <$> g 
        btn <- button "Join Game"
        return $ tagCurrent (view gameInfoName <$> g) btn

    gameList :: forall t m. (MonadWidget t m)
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