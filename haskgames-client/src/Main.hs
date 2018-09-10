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
    import Data.ByteString
    import Control.Monad.Fix
    import qualified Data.Set as Set 

    main = mainWidgetWithCss $(embedFile "static/main.css") $ el "div" $ mdo
        ws <- webSocket "ws://localhost:9000" $ def &
            webSocketConfig_send .~ leftmost [listEvt]
        listEvt <- gameListWidget ws
        return ()
        gamePlayWidget ws 
        return ()

    errorMessage = elClass "div" "error" . text

    gamePlayWidget :: (Reflex t, MonadHold t m, MonadFix m, DomBuilder t m, PostBuild t m)
                     => RawWebSocket t ByteString -> m (Dynamic t ())
    gamePlayWidget ws = do
        gameDyn <- gameStateDyn (ws & _webSocket_recv)
        widgetHold (blank) (updated gameDyn <&> gamePlayInner ws)
        
    gamePlayInner :: (Reflex t, MonadHold t m, MonadFix m, DomBuilder t m, PostBuild t m)
                  => RawWebSocket t ByteString -> Maybe (PublicGame, PersonalState) -> m ()
    gamePlayInner _ Nothing = return ()
    gamePlayInner ws (Just (pg, ps)) = elClass "div" "gameplay-container" $ mdo
        el "div" $ maybe (errorMessage "No call card oh boy") text (judgeCard pg <&> (^. callBody))
        cardSet <- foldDyn toggleElement mempty (leftmost toggleEvents)
        toggleEvents <- elClass "ul" "cards-list" $ mapM (showCard cardSet) (ps ^. personalStateHand)
        return ()

    boolClass t f d = func <$> d
        where
            func v = if v then t else f

    toggleElement e s
        | e `Set.member` s = Set.delete e s
        | otherwise = Set.insert e s
    
    -- TODO: Fix this so it doesn't use a set because ROFL ORDER MATTERS PLEASE KILL ME
    showCard :: forall t m. (Reflex t, MonadHold t m, MonadFix m, DomBuilder t m, PostBuild t m)
             => Dynamic t (Set.Set ResponseCard) -> ResponseCard -> m (Event t ResponseCard)
    showCard cardSet card = do 
        (el, _) <- elDynClass' "li" klass go
        
        let clickEvent = domEvent Click el :: Event t ()
        return $ (const card) <$> clickEvent
        where
            klass = boolClass "card card-selected" "card card-unselected" c
            c = containsElement cardSet card
            go = do
                elClass "h3" "card-title" $ text (card ^. responseBody) 
                
                
        
    containsElement dynSet elm = Set.member elm <$> dynSet

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
    showGame g = el "div" $ do
        el "h1" $ display $ g <&> (^. gameInfoId)
        btn <- el "div" $ do
            el "h2" $ text "Using Decks:"
            el "ul" $
                simpleList (g <&> (^. gameInfoDecks)) display
            btn <- button "Join Game"
            return btn 
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