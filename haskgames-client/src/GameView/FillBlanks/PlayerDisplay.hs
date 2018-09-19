{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , RecursiveDo
           , FlexibleContexts
           , NoMonomorphismRestriction
           , TemplateHaskell
           , GADTs
           , AllowAmbiguousTypes #-}
module GameView.FillBlanks.PlayerDisplay where
    import Reflex.Dom
    import Game.FillBlanks.Game
    import Game.FillBlanks.Event
    import Game.FillBlanks.Deck
    import Game.Basic
    import Control.Lens
    

    playersList :: (MonadWidget t m)
                => Dynamic t PublicGame
                -> m ()
    playersList s = do
        elClass "ul" "pure-u-1 pure-u-md-1-5 players-list" $
            listWithKey (view publicGameActivePlayers <$> s) displayPlayer
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