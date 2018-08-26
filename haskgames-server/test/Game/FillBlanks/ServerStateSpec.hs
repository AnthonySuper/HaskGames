{-# LANGUAGE OverloadedStrings, AllowAmbiguousTypes #-}

module Game.FillBlanks.ServerStateSpec where 
        
    import Test.Hspec
    import Control.Lens
    import Game.FillBlanks.Game
    import Game.FillBlanks.ServerState
    import Game.Common
    import Data.Monoid
    import qualified Data.Map as Map
    import Game.CommonHelpers


    spec :: Spec 
    spec = do
        scoringSpec
        judgementSpec

    defaultPlayers = playersToPlayerState $ map PlayerState [0, 2, 1]
    defaultGame = GameState defaultPlayers $ CommonState "1" 2 mempty mempty

    scoringSpec = do 
        winnerSpec
        increaseScoreSpec

    increaseScoreSpec = describe "increaseScore" $ do
        it "increases if the key exists" $ do
            let ng = increaseScore defaultGame "1"
            let ps = ng ^. playerState . at "1" <&> view playerStateScore :: Maybe Integer
            ps `shouldBe` Just 1
        it "does nothing if the key doesn't" $ do
            increaseScore defaultGame "123123" `shouldBe` defaultGame

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
        describe "increaseJudgementScore" $ do
            let iscore = increaseScoreJudgement gs
            it "modifies if the judgment exists" $
                iscore (JudgementCase [] 1) `shouldNotBe` Just gs
            it "returns nothing if the judgement doesn't exist" $
                iscore (JudgementCase [] 10) `shouldBe` Nothing