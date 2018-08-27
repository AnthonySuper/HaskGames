{-# LANGUAGE OverloadedStrings, AllowAmbiguousTypes #-}

module Game.FillBlanks.ServerSpec where 
        
    import Test.Hspec
    import Control.Lens
    import Game.FillBlanks.Deck
    import Game.FillBlanks.Game
    import Game.FillBlanks.ServerState
    import Game.Common
    import Data.Monoid
    import qualified Data.Map as Map
    import Game.FillBlanks.Server
    import Game.CommonHelpers
    import Game.FillBlanks.Event

    defaultCard = CallCard "" 1 FillIn
    defaultPlayers = playersToPlayerState $ map PlayerState [0, 2, 1, 2]

    makeAwaiting subs =
        GameState defaultPlayers cs
        where
            cs = CommonState "1" 5 mempty sl AwaitingSubmissions defaultCard
            sl = Map.fromList subs



    spec :: Spec 
    spec = do
        waitSubmissionsSpec

        
    waitSubmissionsSpec = describe "while waiting for submissions" $ do
        let oneMore = makeAwaiting $
                        [ (JudgementCase [] 3, "3")
                        , (JudgementCase [] 4, "4") ] 
        let twoMore = makeAwaiting [ (JudgementCase [] 3, "3") ]
        let runTest i p e = runIdentity $ serve i p e
        describe "attempted judgement" $ do
            it "does nothing when sent from the judge" $ do
                let evt = SelectWinner $ JudgementCase [ResponseCard "" FillIn] 1
                runTest twoMore "1" evt `shouldBe` twoMore
        describe "attempted submission" $ do
            describe "sending from somebody yet to submit" $ do
                let jc = JudgementCase [ResponseCard "" FillIn] 2
                let evt = SubmitJudgement $ jc
                describe "when that is the last judgement" $ do
                    let r = runTest oneMore "2" evt
                    it "modifies the state" $
                        r `shouldNotBe` oneMore
                    it "adds the judgement" $ do
                        judgementPlayer r jc `shouldBe` Just "2"
                    it "sets the current status to awaiting judgement" $
                        shouldBe
                            (r ^. commonState . commonStateStatus)
                            AwaitingJudgement
                describe "When there are more judements" $ do
                    let r = runTest twoMore "2" evt
                    it "modifies the state" $ 
                        r `shouldNotBe` twoMore
                    it "adds the judgement" $ do
                        judgementPlayer r jc `shouldBe` Just "2"
                    it "does not change the current status" $
                        shouldBe
                            (r ^. commonState . commonStateStatus)
                            AwaitingSubmissions
            describe "sending from the judge" $ do
                let jc = JudgementCase [ResponseCard "" FillIn] 1
                let evt = SubmitJudgement $ jc
                let r = runTest oneMore "1" evt
                it "does not change the state" $
                    r `shouldBe` oneMore
            describe "sending from a person who already submitted" $ do
                let jc = JudgementCase [ResponseCard "" FillIn] 1
                let evt = SubmitJudgement $ jc
                let r = runTest oneMore "3" evt
                it "does not change the state" $
                    r `shouldBe` oneMore