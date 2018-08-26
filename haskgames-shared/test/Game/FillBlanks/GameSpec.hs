{-# LANGUAGE OverloadedStrings, AllowAmbiguousTypes #-}

module Game.FillBlanks.GameSpec where 
    
import Test.Hspec
import Control.Lens
import Game.FillBlanks.Game
import Game.Common
import Data.Monoid
import qualified Data.Map as Map
import Game.CommonHelpers

spec :: Spec 
spec = do
    describe "FillBlanksState" fillBlanksState



fillBlanksState = do
    describe "fillBlanksWinner" $ do
        let ps = playersToPlayerState $ map PlayerState [0, 2, 1]
        it "works when there is a winner" $ do
            let gs = GameState ps $ CommonState "1" 2 mempty
            fillBlanksWinner gs `shouldBe` Just "2"
        it "works where there is not a winner" $ do
            let gs = GameState ps $ CommonState "1" 3 mempty
            fillBlanksWinner gs `shouldBe` Nothing
            

