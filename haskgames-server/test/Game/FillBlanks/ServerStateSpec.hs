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
    import Data.Maybe (maybe)

    deck' = CardDeck [CallCard ["Test? ", "."] FillIn] [ResponseCard "Test" FillIn]
    deck = mconcat . take 20 . repeat $ deck'
    testCase a = JudgementCase [ResponseCard "Test" FillIn] a

    numResponses g =
        length $ g ^. gameCurrentDeck . cardDeckResponses

    handCardsLength i g = maybe 0 length mhand 
        where
            mhand = g ^? gameActivePlayers . at i . _Just . personalStateHand 

    defaultGame = Game 10 deck deck mempty

    spec :: Spec 
    spec = do
        nextTurnSpec
        judgementCaseSpec
        scoreSpec

    scoreSpec = describe "score handling" $ do
        describe "increaseScore" $ do
            let game' = execState nextTurnScenario defaultGame
            let is = increaseScore game'
            it "works if there is a player" $ do
                let game = is "2"
                game ^? gameActivePlayers . at "2" . _Just . personalStateScore
                    `shouldBe` Just 1 
            it "does nothing if there is not a player" $ do
                let game = is "5"
                game ^? gameActivePlayers . at "5" . _Just . personalStateScore
                    `shouldBe` Nothing

    nextTurnScenario = do
        addPlayer "1"
        addPlayer "2"
        addPlayer "3"
        modify (\x -> addJudgement x "2" $ testCase 2)
        modify (\x -> addJudgement x "3" $ testCase 3)
        modify startJudgement

    judgementCaseSpec = describe "Judgement Case Handling" $ do
        describe "judgementPlayer" $ do
            it "works in a simple case" $ do
                let game = execState nextTurnScenario defaultGame
                judgementPlayer game (testCase 2) `shouldBe` Just "2"
            it "works if there's no player" $ do
                let game = execState nextTurnScenario defaultGame
                judgementPlayer game (testCase 10) `shouldBe` Nothing
        
    nextTurnSpec = do
        let game = execState nextTurnScenario defaultGame
        describe "returnCardsToDeck" $ do
            let gameAfter = execState returnCardsToDeck game 
            it "should return all used responses to the deck" $ do
                let responseLength g = g ^. gameCurrentDeck . cardDeckResponses & length
                -- Each player is dealt 6 cards, but uses 1, so 20 - (2*5) = 10
                responseLength gameAfter `shouldBe` 10
            it "should return all calls to the deck" $ 
                (gameAfter ^. gameCurrentDeck . cardDeckCalls & length)
                    `shouldBe` 20
        describe "dealCards" $ do
            let (c, g) = runState (dealCards 6) game 
            it "should reduce the number of responses" $ 
                numResponses g `shouldNotBe` numResponses game 
            it "should deal the proper number of cards" $ 
                length c `shouldBe` 6 
        describe "dealCardsTo" $ do
            let g = execState (dealCardsTo 6 "2") game
            actsAsDeal g game "2"
        describe "dealNonJudge" $ do
            describe "with a non-judge" $ do
                let g = execState (dealNonJudge 6 "2") game
                actsAsDeal g game "2"
            describe "with a judge" $ do
                let g = execState (dealNonJudge 6 "1") game
                it "should do nothing" $
                    g `shouldBe` game
    
    actsAsDeal g game pid = do
        it "should reduce the number of responses in the deck" $ 
                numResponses g `shouldNotBe` numResponses game
        it "should increase the cards in the player's hand" $ do
            let c = handCardsLength pid 
            c g `shouldNotBe` c game


