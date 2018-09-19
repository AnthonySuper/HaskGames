{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , RecursiveDo
           , FlexibleContexts
           , NoMonomorphismRestriction
           , TemplateHaskell
           , GADTs
           , AllowAmbiguousTypes #-}
module GameView.FillBlanks.GameCreator where
    import Reflex.Dom
    import Reflex.Helpers
    import Reflex.InputWrapper
    import Data.Aeson
    import Game.FillBlanks.Game
    import qualified Data.Text as T
    import qualified Data.Text.Encoding as E
    import Control.Lens 
    import Control.Monad.Fix
    import qualified Data.ByteString.Lazy as BS
    import Data.Monoid ((<>))
    import Data.Functor (($>))
    import Data.List (delete)

    gameCreator :: (MonadWidget t m, DomBuilder t m)
                => m (Event t [BS.ByteString])
    gameCreator = elClass "div" "pure-form pure-form-aligned game-creator-form" $ mdo
        nameInput <- labeledTextInput "Name" "game-name" def
        maxScore <- labeledTextInput "Max Score" "test" $ def 
            & textInputConfig_initialValue .~ "10"
            & textInputConfig_inputType .~ "number"
        decksDyn <- foldDynAp [] decksEvent
        decksEvent <- decksList decksDyn
        btn <- pureButtonClass "Create Game" "pure-button-primary"
        let creationDyn = 
             GameCreator 
                <$> decksDyn 
                <*> (value nameInput) 
                <*> pure Nothing 
                <*> valueAsDefault 10 maxScore
        let createGame = toList . encode . CreateGame <$> creationDyn
        return $ tagCurrent createGame btn
        where
            toList a = [a]

    decksList :: (MonadWidget t m)
              => Dynamic t [T.Text]
              ->  m (Event t ([T.Text] -> [T.Text]))
    decksList decks = elClass "div" "decks-list-container" $ do
        decksDyn <- elClass "ul" "decks-list" $
            simpleList decks displayDeck
        let removeEvt = switch (leftmost <$> current decksDyn)
        addEvent <- elClass "div" "add-container" $ do
            let labelAts = ("for" =: "cardcast-id")
            let fieldAts = ("name" =: "cardcast-id")
            elAttr "label" labelAts $ text "CardCast Deck ID"
            ti <- textInput $ def &
                textInputConfig_attributes .~ pure fieldAts
            btn <- pureButton "Add Card Cast Deck"
            let inputValue = tagValue ti btn 
            return $ toAdd <$> inputValue 
        return $ leftmost [removeEvt, addEvent]
            
        where
            toAdd text deck = deck ++ [text]
        
    displayDeck :: (MonadWidget t m)
                => Dynamic t T.Text
                -> m (Event t ([T.Text] -> [T.Text]))
    displayDeck deck = elClass "li" "decks-list-item" $ do
        elClass "span" "decks-name" $ dynText deck
        b <- button "Remove"
        return $ tag removeBehavior b 
        where
            removeBehavior = toRemove <$> current deck
            toRemove d = delete d 
    

