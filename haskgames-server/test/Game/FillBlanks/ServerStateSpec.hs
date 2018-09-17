{-# LANGUAGE OverloadedStrings
            , AllowAmbiguousTypes
            , NoMonomorphismRestriction
            , FlexibleContexts #-}

module Game.FillBlanks.ServerStateSpec where 
        
    import Test.Hspec
    import Control.Lens
    import Game.FillBlanks.Deck
    import Game.FillBlanks.Game
    import Game.FillBlanks.ServerState
    import Game.Common
    import Data.Monoid
    import qualified Data.Map as Map
    import Game.CommonHelpers
    import Control.Monad.State

    deck' = CardDeck [CallCard ["Test? ", "."] FillIn] [ResponseCard "Test" FillIn]
    deck = mconcat . take 20 . repeat $ deck'
    testCase a = JudgementCase [ResponseCard "Test" FillIn] a

    defaultGame = Game 10 deck deck mempty

    spec :: Spec 
    spec = do
        nextTurnSpec


    nextTurnScenario = do
        addPlayer "1"
        addPlayer "2"
        addPlayer "3"
        modify (\x -> addJudgement x "2" $ testCase 2)
        modify (\x -> addJudgement x "3" $ testCase 3)
        modify startJudgement 


    
    nextTurnSpec = do
        let game = execState nextTurnScenario defaultGame
        describe "returnCardsToDeck" $ do
            let gameAfter = execState returnCardsToDeck game 
            it "should return all used responses to the deck" $ do 
                let responseLength g = g ^. gameCurrentDeck . cardDeckResponses & length
                -- Each player is dealt 6 cards, but uses 1, so 20 - (2*5) = 10
                responseLength gameAfter `shouldBe` 10
            it "should return all calls to the deck" $ do
                (gameAfter ^. gameCurrentDeck . cardDeckCalls & length)
                    `shouldBe` 20

