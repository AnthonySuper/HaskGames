{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , RecursiveDo
           , FlexibleContexts
           , NoMonomorphismRestriction
           , TemplateHaskell
           , GADTs #-}
module GameView.FillBlanks.GamePlay where
    import Reflex.Dom
    import Game.FillBlanks.Game
    import Game.FillBlanks.Event
    import Game.FillBlanks.Deck
    import Game.Basic
    import Data.Aeson
    import qualified Data.Text as T
    import Control.Lens
    import Control.Monad.Fix
    import Control.Applicative (liftA2)
    import Data.Functor (($>), (<$))
    import Data.ByteString (ByteString)
    import qualified Data.ByteString.Lazy as BS
    import qualified Data.Map as Map
    import Data.Maybe (maybe)
    import Data.List (delete)


    gamePlayWidget :: forall t m . (MonadWidget t m)
                   => WebSocket t
                   -> m (Event t [BS.ByteString])
    gamePlayWidget ws = elClass "div" "play-container" $ do
        maybeState <- (gameStateDyn $ ws & _webSocket_recv)
        state <- maybeDyn maybeState
        dyn $ gamePlayM <$> state
        return never

    gamePlayM :: forall t m. (MonadWidget t m)
              => Maybe (Dynamic t (PublicGame, PersonalState))
              -> m ()
    gamePlayM Nothing = return ()
    gamePlayM (Just s) = gamePlayInner s

    gamePlayInner :: forall t m . (MonadWidget t m)
                  => Dynamic t (PublicGame, PersonalState)
                  -> m ()
    gamePlayInner state = do
        playerState <- holdUniqDyn $ view _2 <$> state
        callCard <- holdUniqDyn $ judgeCard <$> view _1 <$> state
        elClass "ul" "players-list" $ 
            listWithKey (view (_1 . publicGameActivePlayers) <$> state) displayPlayer
        elClass "div" "gameplay-area" $ do
            dyn $ displayHand <$> callCard <*> playerState
        return ()

    displayPlayer :: (MonadWidget t m)
                  => PlayerId
                  -> Dynamic t ImpersonalState
                  -> m ()
    displayPlayer name val = elClass "li" "players-list-item" $ do
        el "h3" $ text name
        elClass "div" "players-list-score" $ do
            text "Score: "
            display $ view impersonalStateScore <$> val
        elClass "div" "players-list-status" $
            dynText $ showStatus <$> view impersonalStateStatus <$> val  
        return ()
        where
            showStatus (Selector SelectingCards) = "Selecting Cards"
            showStatus (Selector (WaitingJudgement ())) = "Submitted"
            showStatus SittingOut = "Sitting Out"
            showStatus (Judge (WaitingCases _)) = "Waiting Submissions"
            showStatus (Judge (PickingWinner _ _)) = "Picking Winner"
        
    gameStateDyn :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
                 => Event t ByteString
                 -> m (Dynamic t (Maybe (PublicGame, PersonalState)))
    gameStateDyn e = foldDyn folder Nothing encoded
        where
            folder (UpdateState pg ps) _ = Just (pg, ps)
            folder _ n = n
            encoded :: Event t ServerEvent
            encoded = fmapMaybe decodeStrict e

    displayHand :: (MonadWidget t m)
                => Maybe CallCard
                -> PersonalState
                -> m ()
    displayHand call s = elClass "div" "hand-display" $ mdo
        handDyn <- foldDyn ($) [] removeCards
        -- Eventually make this be an event that returns a card lmao
        addCards <- elClass "ul" "hand-cards-list" $
            mapM displayResponseCard (s ^. personalStateHand)
        resp <- dyn $ responseInContext callBody' <$> handDyn
        removeCards <- switchHold (never $> id) resp 
        return ()
        where
            callBody' :: [T.Text]
            callBody' = maybe [] (^. callBody) call

    displayResponseCard :: (MonadWidget t m)
                        => ResponseCard
                        -> m ()
    displayResponseCard c = elClass "li" "response-card" $ do
        el "h3" $ text (c ^. responseBody)
        return ()

    responseInContext :: forall t m. (MonadWidget t m)
                      => [T.Text]
                      -> [ResponseCard]
                      -> m (Event t ([ResponseCard] -> [ResponseCard]))
    responseInContext = go
        where
            go [] _ = return never
            go (c:cs) [] = do
                elClass "span" "answer-call-segment" $ text c
                go cs [] 
            go (c:cs) (r:rs) = do
                elClass "span" "answer-call-segment" $ text c
                (e, _) <- elAttr' "span" ("class" =: "answer-resp-segemnt") $ text c
                rest <- go cs rs
                return $ leftmost [domEvent Click e $> delete r, rest]

