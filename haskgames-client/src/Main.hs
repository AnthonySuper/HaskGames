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

    main = mainWidgetWithCss $(embedFile "static/main.css") $ el "div" $ mdo
        ws <- webSocket "ws://localhost:9000" $ def &
            webSocketConfig_send .~ leftmost [listEvt]
        listEvt <- fullWorkflow ws
        return ()

    {-
    errorMessage = elClass "div" "error" . text

    gamePlayWidget :: (Reflex t, MonadHold t m, MonadFix m, DomBuilder t m, PostBuild t m)
                     => WebSocket t -> m (Dynamic t ())
    gamePlayWidget ws = do
        gameDyn <- gameStateDyn (ws & _webSocket_recv)
        widgetHold (blank) (updated gameDyn <&> gamePlayInner ws)
        
    gamePlayInner :: (Reflex t, MonadHold t m, MonadFix m, DomBuilder t m, PostBuild t m)
                  => WebSocket t -> Maybe (PublicGame, PersonalState) -> m ()
    gamePlayInner _ Nothing = return ()
    gamePlayInner ws (Just (pg, ps)) = elClass "div" "gameplay-container" $ mdo
        el "div" $ text "Type me later lol"
        let keys = Map.keys (pg ^. publicGameActivePlayers)
        el "div" $ text $ T.intercalate ", " keys
        cardSet <- foldDyn toggleElement mempty (leftmost toggleEvents)
        toggleEvents <- elClass "ul" "cards-list" $ mapM (showCard cardSet) (ps ^. personalStateHand)
        return ()

    boolClass t f d = func <$> d
        where
            func v = if v then t else f

    toggleElement e s
        | e `elem` s = delete e s
        | otherwise = e : s
    
    -- TODO: Fix this so it doesn't use a set because ROFL ORDER MATTERS PLEASE KILL ME
    showCard :: forall t m. (Reflex t, MonadHold t m, MonadFix m, DomBuilder t m, PostBuild t m)
             => Dynamic t [ResponseCard] -> ResponseCard -> m (Event t ResponseCard)
    showCard cardSet card = do 
        (el, _) <- elDynClass' "li" klass go
        
        let clickEvent = domEvent Click el :: Event t ()
        return $ (const card) <$> clickEvent
        where
            klass = boolClass "card card-selected" "card card-unselected" c
            c = containsElement cardSet card
            go = do
                elClass "h3" "card-title" $ text (card ^. responseBody) 
        
    containsElement dynSet elm = elem elm <$> dynSet

    gameStateDyn :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
                 => Event t ByteString
                 -> m (Dynamic t (Maybe (PublicGame, PersonalState)))
    gameStateDyn e = foldDyn folder Nothing encoded
        where
            folder (UpdateState pg ps) _ = Just (pg, ps)
            folder _ n = n
            encoded :: Event t ServerEvent
            encoded = fmapMaybe decodeStrict e
    -}