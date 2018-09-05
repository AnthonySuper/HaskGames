{-# LANGUAGE OverloadedStrings, AllowAmbiguousTypes #-}

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


    spec :: Spec 
    spec =
        scoringSpec

    defaultPlayers = playersToPlayerState $ map Player [0, 2, 1]
    defaultGame = Game "1" 2 mempty mempty mempty AwaitingSubmissions defaultCard defaultPlayers
    defaultCard = Just $ CallCard "" 1 FillIn

    scoringSpec = do 
        judgementSpec
        increaseScoreSpec

    increaseScoreSpec = describe "increaseScore" $ do
        it "increases if the key exists" $ do
            let ng = increaseScore defaultGame "1"
            let ps = ng ^. gameActivePlayers . at "1" <&> view playerScore :: Maybe Integer
            ps `shouldBe` Just 1
        it "does nothing if the key doesn't" $ do
            increaseScore defaultGame "123123" `shouldBe` defaultGame
                
    judgementSpec = do
        let cs = Map.fromList [ (JudgementCase [] 1, "1")
                            , (JudgementCase [] 2, "2")
                            , (JudgementCase [] 3, "3") ]
        let gs = Game "1" 3 mempty mempty cs AwaitingSubmissions Nothing defaultPlayers

        describe "judgementPlayer" $ do
            let jp = judgementPlayer gs
            it "finds the player if the player exists" $ 
                jp (JudgementCase [] 1) `shouldBe` Just "1"
            it "returns nothing if the player does not exist" $
                jp (JudgementCase [] 10) `shouldBe` Nothing
        describe "increaseJudgementScore" $ do
            let iscore = increaseScoreJudgement gs
            it "modifies the state if the player exists" $
                iscore (JudgementCase [] 1) `shouldNotBe` Just gs
            it "returns nothing if the judgement doesn't exist" $
                iscore (JudgementCase [] 10) `shouldBe` Nothing
        describe "judgeable" $ do
            it "is true if there are enough cases" $
                judgeable gs `shouldBe` True
            it "is false if there aren't enough cases" $
                judgeable defaultGame `shouldBe` False