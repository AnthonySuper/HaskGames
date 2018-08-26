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
    winnerSpec
    judgementSpec

defaultPlayers = playersToPlayerState $ map PlayerState [0, 2, 1]

winnerSpec = describe "winner" $ do
    it "works when there is a winner" $ do
        let gs = GameState defaultPlayers $ CommonState "1" 2 mempty mempty
        winner gs `shouldBe` Just "2"
    it "works where there is not a winner" $ do
        let gs = GameState defaultPlayers $ CommonState "1" 3 mempty mempty
        winner gs `shouldBe` Nothing
            
judgementSpec = describe "judgement cases" $ do
    let cs = Map.fromList [ (JudgementCase [] 1, "1")
                          , (JudgementCase [] 2, "2")
                          , (JudgementCase [] 3, "3") ]
    let gs = GameState defaultPlayers $ CommonState "1" 3 mempty cs

    describe "judgementPlayer" $ do
        let jp = judgementPlayer gs
        it "finds if it exists" $ 
            jp (JudgementCase [] 1) `shouldBe` Just "1"
        it "does not find if it does not exist" $
            jp (JudgementCase [] 10) `shouldBe` Nothing