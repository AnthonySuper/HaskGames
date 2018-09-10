{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecursiveDo #-}
    module Main where
    import Reflex.Dom
    import Data.Monoid
    import Game.FillBlanks.Game
    import Game.FillBlanks.Event
    import Game.FillBlanks.Deck
    import Data.Aeson
    import qualified Data.Text as T
    import Data.Text.Encoding as E
    import Control.Lens 
    import Data.ByteString
    import Control.Monad.Fix

    main = mainWidget $ el "div" $ mdo
        ws <- webSocket "ws://localhost:9000" $ def &
            webSocketConfig_send .~ leftmost [listEvt]
        listEvt <- gameListWidget ws
        gamePlayWidget ws 
        return ()

    gamePlayWidget ws = do
        gameDyn <- gameStateDyn (ws & _webSocket_recv)
        dyn $ gamePlayInner ws <$> gameDyn

    gamePlayInner _ Nothing = return ()
    gamePlayInner ws (Just (pg, ps)) = do
        el "h1" $ text "Gameplay wow"
        el "ul" $ mapM_ showCard (ps ^. personalStateHand)
        return ()


    showCard card = el "li" $ text (card ^. responseBody)

    gameListWidget ws = el "div" $ do
        ds <- gameList $ traceEvent "WS RECV:" (ws & _webSocket_recv)
        joinEvents <- el "ul" $ simpleList ds showGame
        let selectEvent = switchPromptlyDyn (joinEvents <&> leftmost)
        display =<< holdDyn (-1) selectEvent
        b <- button "Refresh List"
        let broadcastEvt = leftmost [ const [(encode ListGames)] <$> b
                                    , pure . encode . JoinGame <$> selectEvent
                                    ]
        return broadcastEvt
        
    showGame :: (Reflex t, MonadHold t m, MonadFix m, DomBuilder t m, PostBuild t m)
             => Dynamic t GameInfo -> m (Event t Int)
    showGame g = el "li" $ do
        el "h1" $ display (g <&> (^. gameInfoId))
        btn <- el "div" $ do
            el "h2" $ text "Using Decks:"
            el "ul" $
                simpleList (g <&> (^. gameInfoDecks)) display
            button "Join Game"
        return $ tagPromptlyDyn (g <&> (^. gameInfoId)) btn

    gameStateDyn :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
                 => Event t ByteString
                 -> m (Dynamic t (Maybe (PublicGame, PersonalState)))
    gameStateDyn e = foldDyn folder Nothing encoded
        where
            folder (UpdateState pg ps) _ = Just (pg, ps)
            folder _ n = n
            encoded :: Event t ServerEvent
            encoded = fmapMaybe decodeStrict e

    gameList :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
             => Event t ByteString
             -> m (Dynamic t [GameInfo])
    gameList e = foldDyn folder [] encoded
        where
            folder (ReadInfo info) _ = info
            folder _ s = s 
            encoded :: Event t CoordinationResponse
            encoded = fmapMaybe decodeStrict e