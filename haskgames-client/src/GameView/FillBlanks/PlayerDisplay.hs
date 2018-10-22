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
    playersList s = elClass "div" "column is-one-quarter" $ do
        elClass "ul" "tile is-ancestor is-vertical" $
            listWithKey (view publicGameActivePlayers <$> s) displayPlayer
        return ()

    displayPlayer :: (MonadWidget t m)
                  => PlayerId
                  -> Dynamic t ImpersonalState
                  -> m ()
    displayPlayer name val = elClass "li" "tile is-vertical" $ do
        elClass "h6" "title is-4 has-text-centered" $ text name 
        elClass "nav" "level subtitle" $ do
            elClass "div" "level-left" $ do
                elClass "div" "has-text-centered" $ do
                    elClass "p" "heading" $ text "Score"
                    elClass "p" "title is-6" $
                        display $ view impersonalStateScore <$> val
            elClass "div" "level-right" $
                elClass "div" "has-text-centered" $ do
                    elClass "p" "heading" $ text "Status"
                    elClass "p" "title is-6" $
                        dynText $ showStatus <$> view impersonalStateStatus <$> val  
        return ()
        where
            showStatus (Selector SelectingCards) = "Selecting"
            showStatus (Selector (WaitingJudgement _)) = "Submitted"
            showStatus SittingOut = "Sitting Out"
            showStatus (Judge (WaitingCases _)) = "Waiting"
            showStatus (Judge (PickingWinner _ _)) = "Judging"