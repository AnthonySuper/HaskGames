{-# LANGUAGE OverloadedStrings #-}

module Game.FillBlanks.CardCastSpec where 
    
import Test.Hspec

import Game.FillBlanks.CardCast
import Game.FillBlanks.CardCast.Internal
import Game.FillBlanks.Deck
import Control.Lens
import Data.Maybe

spec :: Spec 
spec = do
    describe "Converting call cards" $ do
        let call = Call ["What's in the box? ", "."] "asdf"
        let converted = callToGame call
        it "properly sets the body" $
            converted ^. callBody `shouldBe` ["What's in the box? ", "."]
        it "properly sets the source" $
            converted ^. callSource `shouldBe` CardCastDeck "asdf"
    describe "Converting response cards" $ do 
        let response = Response "test" "asdf"
        let converted = responseToGame response
        it "properly sets the text" $
            converted ^. responseBody `shouldBe` "test"
        it "properly sets the id" $
            converted ^. responseSource `shouldBe` CardCastDeck "asdf"
    describe "Getting cards from the internet" $ do 
        let getCards = getCardDeck "7BH6T"
        it "parses properly" $ do 
            cards <- getCards
            cards `shouldSatisfy` isJust