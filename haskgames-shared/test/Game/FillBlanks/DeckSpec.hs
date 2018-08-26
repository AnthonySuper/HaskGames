{-# LANGUAGE OverloadedStrings, AllowAmbiguousTypes #-}

module Game.FillBlanks.DeckSpec where 
    
import Test.Hspec
import Control.Lens
import Game.FillBlanks.Deck
import Data.Monoid

exResp = ResponseCard "Test" FillIn
exCall = CallCard "What test? _." 1 FillIn
exDeck = CardDeck [exCall] [exResp]

spec = do 
    describe "semigroup instance" $
        it "validly combines empty" $ do
            let empty = CardDeck [] []
            empty <> empty `shouldBe` empty
    describe "monoid instance" $ do
        it "has a valid empty" $ do
            let c = mempty :: CardDeck
            c <> c `shouldBe` c
        it "appens with empty" $
            exDeck <> mempty `shouldBe` exDeck
        it "mconcats validly" $
            shouldBe
                (mconcat [exDeck, exDeck, exDeck])
                (exDeck <> exDeck <> exDeck)
