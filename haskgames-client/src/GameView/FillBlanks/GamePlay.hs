{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , RecursiveDo
           , FlexibleContexts
           , NoMonomorphismRestriction
           , TemplateHaskell
           , GADTs
           , AllowAmbiguousTypes #-}
module GameView.FillBlanks.GamePlay where
    import Reflex.Dom
    import Reflex.Helpers
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
    import Data.Foldable
    import qualified Data.ByteString.Lazy as BS
    import qualified Data.Map as Map
    import Data.Monoid
    import Control.Monad (when, (>=>))
    import Data.Maybe (maybe, isJust)
    import Data.List (delete)
    import GameView.FillBlanks.HandSelector
    import GameView.FillBlanks.PlayerDisplay
    import GameView.FillBlanks.ChatView

    gamePlayWidget :: forall t m . (MonadWidget t m)
                   => WebSocket t
                   -> m (Event t [BS.ByteString])
    gamePlayWidget ws = do
        let serverEvents = serverEvent $ ws & _webSocket_recv
        maybeState <- gameStateDyn serverEvents
        state <- maybeDyn maybeState
        el "article"  $ do
            socketDyn <- dynHold $ gamePlayM <$> state
            ce <- chatView serverEvents
            return $ ce <> socketDyn

    gamePlayM :: forall t m. (MonadWidget t m)
              => Maybe (Dynamic t (PublicGame, PersonalState))
              -> m (Event t [BS.ByteString])
    gamePlayM Nothing = return never
    gamePlayM (Just s) = gamePlayInner s


    sectionOfColumns = 
        elClass "section" "section gameplay-section" . elClass "div" "columns"

    gamePlayInner :: forall t m . (MonadWidget t m)
                  => Dynamic t (PublicGame, PersonalState)
                  -> m (Event t [BS.ByteString])
    gamePlayInner state = sectionOfColumns $ do
        playerState <- holdUniqDyn $ view _2 <$> state
        publicState <- holdUniqDyn $ view _1 <$> state 
        callCard <- holdUniqDyn $ judgeCard <$> view _1 <$> state
        playersList (view _1 <$> state)
        broadcastE <- elClass "div" "column" $ do
            je <- judgingArea callCard playerState publicState
            se <- dynHold $ selectingArea <$> callCard <*> playerState
            return $ leftmost [se, je]
        return broadcastE

    judgingArea :: MonadWidget t m
                => Dynamic t (Maybe CallCard)
                -> Dynamic t PersonalState
                -> Dynamic t PublicGame
                -> m (Event t [BS.ByteString])
    judgingArea call ps pg = elClass "div" "judgement-area" $ mdo
        selected <- holdDyn Nothing (Just <$> selEvt) 
        selEvts <- simpleList (judgementCases <$> pg) $ showJudgementCase call selected
        let selEvt = switch (current $ leftmost <$> selEvts) -- Event t JudgementCase
        sendEvent <- whenDynHold (canJudge <$> ps <*> selected) never $ 
            button "Select"
        return $ tagCurrent (winnerEvt <$> selected) sendEvent 
        where 
            winnerEvt Nothing = []
            winnerEvt (Just a) = [encode . SelectWinner $ a]
            canJudge ps sel = isJudge ps && (isJust sel)

    showJudgementCase :: MonadWidget t m
                      => Dynamic t (Maybe CallCard)
                      -> Dynamic t (Maybe JudgementCase) 
                      -> Dynamic t JudgementCase
                      -> m (Event t JudgementCase)
    showJudgementCase card sel jc = elClass "div" "judgement-case" $ do
        dynHold $ judgementResponse <$> callBody' <*> eqA <*> jc
        where
            callBody' = maybe [] (^. callBody) <$> card
            eqA = liftA2 (==) sel (Just <$> jc)

    judgementResponse :: (MonadWidget t m)
                      => [T.Text]
                      -> Bool 
                      -> JudgementCase
                      -> m (Event t JudgementCase)
    judgementResponse body sel jc = do
        let attrs = ("class" =: klass)
        (el, _) <- elAttr' "span" attrs $ 
            responseInContext body (jc ^. judgementCaseCards)
        return $ domEvent Click el $> jc
        where
            klass = 
                if sel then
                    "judgement-selected"
                else
                    ""
        
    selectingArea :: MonadWidget t m
                  => Maybe CallCard
                  -> PersonalState
                  -> m (Event t [BS.ByteString])
    selectingArea card state = case state ^. personalStateStatus of
        Selector SelectingCards -> do 
            judgeSelect <- handSelector card state
            return $ toMessage . SubmitJudgement <$> judgeSelect 
        Selector (WaitingJudgement jc) -> do 
            judgementWaiter card jc
            return never
        _ -> return never

    judgementWaiter :: MonadWidget t m
                    => Maybe CallCard
                    -> JudgementCase
                    -> m () 
    judgementWaiter call jc = elClass "div" "judgement-waiter" $ do
        el "h3" $ text "Awaiting Judgement..."
        el "h4" $ text "Your response was: "
        responseInContext callBody' (jc ^. judgementCaseCards)
        return ()
        where
            callBody' :: [T.Text]
            callBody' = maybe [] (^. callBody) call 

    serverEvent :: forall t. (Reflex t)
                => Event t ByteString
                -> Event t ServerEvent
    serverEvent = fmapMaybe decodeStrict

    gameStateDyn :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
                 => Event t ServerEvent
                 -> m (Dynamic t (Maybe (PublicGame, PersonalState)))
    gameStateDyn e = foldDyn folder Nothing e
        where
            folder (UpdateState pg ps) _ = Just (pg, ps)
            folder _ n = n

    handSelector :: (MonadWidget t m)
                 => Maybe CallCard
                 -> PersonalState
                 -> m (Event t JudgementCase)
    handSelector call s = elClass "div" "hand-display" $ mdo
        handDyn <- foldDynAp [] $ leftmost [removeCards, addCards]
        removeCards <- dynHold $ responseInContext callBody' <$> handDyn
        submitEvt <- whenDynHold fullHand never $ button "Submit for Judgement"
        addCards <- dynHold $ 
            responseCardSection callArity <$> handDyn <*> pure (s ^. personalStateHand)
        let judgementDyn = JudgementCase <$> handDyn <*> pure 102
        let fullHand = (callArity - 1 ==) . length <$> handDyn 
        return $ tagCurrent judgementDyn submitEvt
        where
            callArity = length callBody'
            callBody' :: [T.Text]
            callBody' = maybe [] (^. callBody) call
    
    responseInContext :: forall t m. (MonadWidget t m)
                      => [T.Text]
                      -> [ResponseCard]
                      -> m (Event t ([ResponseCard] -> [ResponseCard]))
    responseInContext c r = 
        elClass "div" "card card-2 response-display color-highlight-two" $ go c r
        where
            go [] _ = return never
            go (c:cs) [] = do
                elClass "span" "answer-call-segment" $ text c
                when (cs /= []) $ elClass "span" "answer-resp-empty" $ text "_"
                go cs []
            go (c:cs) (r:rs) = do
                elClass "span" "answer-call-segment" $ text c
                (e, _) <- elAttr' "span" ("class" =: "answer-resp-segment") $ 
                    text (r ^. responseBody)
                rest <- go cs rs
                return $ (domEvent Click e $> delete r) `mappend`rest

