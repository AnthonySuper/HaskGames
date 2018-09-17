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
    import Data.Maybe (maybe)
    import Data.List (delete)
    import GameView.FillBlanks.HandSelector
    import GameView.FillBlanks.PlayerDisplay

    gamePlayWidget :: forall t m . (MonadWidget t m)
                   => WebSocket t
                   -> m (Event t [BS.ByteString])
    gamePlayWidget ws = elClass "div" "play-container" $ do
        maybeState <- (gameStateDyn $ ws & _webSocket_recv)
        state <- maybeDyn maybeState
        socketDyn <- dyn $ gamePlayM <$> state
        actEvent <- switchHold never socketDyn
        return actEvent

    gamePlayM :: forall t m. (MonadWidget t m)
              => Maybe (Dynamic t (PublicGame, PersonalState))
              -> m (Event t [BS.ByteString])
    gamePlayM Nothing = return never
    gamePlayM (Just s) = gamePlayInner s

    gamePlayInner :: forall t m . (MonadWidget t m)
                  => Dynamic t (PublicGame, PersonalState)
                  -> m (Event t [BS.ByteString])
    gamePlayInner state = do
        playerState <- holdUniqDyn $ view _2 <$> state
        callCard <- holdUniqDyn $ judgeCard <$> view _1 <$> state
        playersList (view _1 <$> state)
        submitE <- elClass "div" "gameplay-area" $ do
            dynHold $ displayHand <$> callCard <*> playerState
        return $ toList . encode . SubmitJudgement <$> submitE 
        where
            toList a = [a]

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
                -> m (Event t JudgementCase)
    displayHand call s = elClass "div" "hand-display" $ mdo
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
    responseInContext c r = elClass "div" "card card-2 response-display" $ go c r
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

